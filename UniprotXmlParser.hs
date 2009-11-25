{-# LANGUAGE
  Arrows
  , BangPatterns
  #-}
module UniprotXmlParser ( Entry(..)
                        , Interaction(..)
                        , CSV(..)
                        , same
                        , has
                        , run
                         ) where

import Control.Applicative
import Control.Monad (sequence)
import Control.Parallel (par,pseq)
import Control.Parallel.Strategies (parMap, rwhnf, rnf, NFData)
import Data.Char (toUpper)
import Data.Either
import Data.List (intercalate, isInfixOf)
import Data.Maybe
import Data.Ord (comparing)
import System.Environment
import Text.XML.HXT.Arrow
import System.IO.Unsafe
import System.FilePath
import System.Posix.Files
import System.IO


data Entry = Entry {
      accession
    , entry
    , protein
    , organism
        :: String
    } deriving (Eq, Show)

instance NFData Entry where
    rnf (Entry { accession=n, entry=e, protein=p, organism=o }) = n `par` e `par` p `par` o `par` ()

data Interaction = Interaction {
      origin
    , target
        :: Entry
    , score :: Double
    } deriving (Eq, Show)

instance NFData Interaction where
    rnf (Interaction { origin = o, target = t, score = s }) = o `par` t `par` s `par` ()

class CSV a where
    csv :: a -> String

instance CSV Entry where
    csv = csvEntry
        where csvEntry :: Entry -> String
              csvEntry e = intercalate ";" [accession e, entry e, protein e, organism e]

instance CSV Interaction where
    csv = csvInteraction
        where csvInteraction :: Interaction -> String
              csvInteraction i = intercalate ";" [csv $ origin i, csv $ target i, show $ score i]

parseToEntry = atTag "entry" >>>
               proc e -> do
                 acc   <- single accession    -< e
                 ename <- entryName           -< e
                 p     <- proteinName         -< e
                 oname <- single organismName -< e
                 returnA -< Entry {
                               accession = acc
                             , entry     = ename
                             , protein   = p
                             , organism  = oname }
    
    where

      atTag tag            = deep (isElem >>> hasName tag)
      atTag' tag           = isElem       >>> hasName tag
      tagData              = getChildren  >>> getText
      downFrom top a       = hasName top /> a
      extractFrom mk top a = arr mk <<< tagData <<< downFrom top a
      extractFromEntry mk  = extractFrom mk "entry"


      accession            = extractFromEntry id $ hasName "accession"
      entryName            = extractFromEntry id $ hasName "name"
      proteinName          = single fullNames
      fullNames            = hasName "entry"  /> hasName "protein" >>> atTag "fullName" >>> tagData
      organismName         = atTag "organism" /> hasName "name"    >>> tagData


type ErrorMsg = String
mkInteraction :: FilePath -> [String] -> IO (Either ErrorMsg Interaction)
mkInteraction dir [ori,tar,prob] = do

  let uniprots@[ori',tar'] = map uniprot [ori,tar]

  oks@[ori_ok,tar_ok] <- mapM fileExist uniprots

  if and oks
     then let res = parMap rwhnf (\xml -> runX (readDocument [] xml >>> parseToEntry)) uniprots
          in (Right . flip mkInteraction' (read prob)) <$> sequence res
     else return . Left $
          if not ori_ok
             then badEntry ori'
             else badEntry tar'

             where uniprot id = dir </> id <.> "xml"

                   mkInteraction' :: [[Entry]] -> Double -> Interaction
                   mkInteraction' [[o],[t]] p = Interaction { origin = o, target = t, score = p }

                   badEntry = ("Cannot find " ++)



interspecies :: Interaction -> Bool
interspecies i = if same "Plasmodium" i ||
                    same "Human"      i
                 then False
                 else True

same :: String -> Interaction -> Bool
same s i = hasBoth organism s (origin i) (target i)

hasBoth :: (Entry -> String) -> String -> Entry -> Entry -> Bool
hasBoth f s a b = has f s a && has f s b

has :: (Entry -> String) -> String -> Entry -> Bool
has f s e = (up s) `isInfixOf` (up $ f e)
    where up :: String -> String
          up = parMap rnf toUpper



rearrange :: Interaction -> Interaction
rearrange i = if has organism "Plasmodium" (origin i)
              then i { origin = target i, target = origin i }
              else i


run :: (Interaction -> Interaction) -> (Interaction -> Bool) -> IO ()
run fixer filterFunc = do
  [dir]              <- getArgs
  interactions       <- (parMap rnf words . filter (not . null) . lines) <$> getContents
  (failed,succeeded) <- partitionEithers <$> mapM (mkInteraction dir) interactions
  print' stdout csv . parMap rnf fixer . filter filterFunc $ succeeded
  print' stderr id  failed
    where print' h f = mapM_ (hPutStrLn h . f)
