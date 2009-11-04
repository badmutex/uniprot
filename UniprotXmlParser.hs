{-# LANGUAGE
  Arrows
  , BangPatterns
  #-}
module Main where

import Control.Applicative
import Control.Monad (sequence)
import Control.Parallel.Strategies (parMap, rwhnf, rnf)
import Data.Char (toUpper)
import Data.List (intercalate, isInfixOf)
import Data.Ord (comparing)
import System.Environment
import Text.XML.HXT.Arrow
import System.IO.Unsafe

data Entry = Entry {
      accession
    , entry
    , protein
    , organism
        :: String
    } deriving (Eq, Show)

data Interaction = Interaction {
      origin
    , target
        :: Entry
    , score :: Double
    } deriving (Eq, Show)

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


mkInteraction :: [String] -> IO Interaction
mkInteraction [ori,tar,prob] =
    let ori' = uniprot ori
        tar' = uniprot tar
        res = parMap rwhnf (\xml -> runX (readDocument [] xml >>> parseToEntry)) [ori', tar']
    in (rearrange . flip mkInteraction' (read prob)) <$> sequence (res `seq` res)

        where uniprot id = "http://www.uniprot.org/uniprot/" ++ id ++ ".xml"
              
              rearrange :: Interaction -> Interaction
              rearrange i = if has organism "Plasmodium" (origin i)
                            then i { origin = target i, target = origin i }
                            else i

              mkInteraction' :: [[Entry]] -> Double -> Interaction
              mkInteraction' [[o],[t]] p = Interaction { origin = o, target = t, score = p }

interspecies :: Interaction -> Bool
interspecies i = if same "Plasmodium" i ||
                    same "Human"      i
                 then False
                 else True

    where same :: String -> Interaction -> Bool
          same s i = hasBoth organism s (origin i) (target i)

hasBoth :: (Entry -> String) -> String -> Entry -> Entry -> Bool
hasBoth f s a b = has f s a && has f s b

has :: (Entry -> String) -> String -> Entry -> Bool
has f s e = (up s) `isInfixOf` (up $ f e)
    where up :: String -> String
          up = map toUpper


main = do
  let self_interaction [ori,tar,prob] = if ori == tar then False else True

  interactions <- (filter self_interaction . parMap rnf words . lines) <$> getContents
  names        <- (filter interspecies) <$> mapM mkInteraction interactions
  mapM_ (putStrLn . csv) names
