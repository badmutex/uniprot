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

-- | In lieu of comma-separated values since there may be a clash within the
--  uniprot xml file (I'm looking at you Q8I528), separate them by pipe symbols |
class LSV a where
    lsv :: a -> String

svEntry v e = intercalate v [ accession e
                            , entry     e
                            , protein   e
                            , organism  e
                            ]

instance CSV Entry where
    csv = svEntry ";"

instance LSV Entry where
    lsv = svEntry "|"

svInteraction v i = intercalate v [ svEntry v $ origin i
                                  , svEntry v $ target i
                                  , show      $ score  i
                                  ]

instance CSV Interaction where
    csv = svInteraction ";"

instance LSV Interaction where
    lsv = svInteraction "|"

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


same :: String -> Interaction -> Bool
same s i = hasBoth organism s (origin i) (target i)

hasBoth :: (Entry -> String) -> String -> Entry -> Entry -> Bool
hasBoth f s a b = has a f s && has b f s


-- | Extract a value from an Entry and compare it to a string
-- > has entry organism "Plasmodium"
has :: Entry -> (Entry -> String) -> String -> Bool
has e f s = (up s) `isInfixOf` (up $ f e)
    where up :: String -> String
          up = parMap rnf toUpper


run :: (Interaction -> Interaction) -> (Interaction -> Bool) -> IO ()
run fixer filterFunc = do
  [dir]              <- getArgs
  interactions       <- (parMap rnf words . filter (not . null) . lines) <$> getContents
  (failed,succeeded) <- partitionEithers <$> mapM (mkInteraction dir) interactions
  print' stdout lsv . parMap rnf fixer . filter filterFunc $ succeeded
  print' stderr id  failed
    where print' h f = mapM_ (hPutStrLn h . f)
