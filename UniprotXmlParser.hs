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
      _accession
    , _entry
    , _protein
    , _organism
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

csvEntry :: Entry -> String
csvEntry e = intercalate ";" [_accession e, _entry e, _protein e, _organism e]

instance CSV Entry where csv = csvEntry

csvInteraction :: Interaction -> String
csvInteraction i = intercalate ";" [csv $ origin i, csv $ target i, show $ score i]

instance CSV Interaction where csv = csvInteraction

atTag tag = deep (isElem >>> hasName tag)
atTag' tag = isElem      >>> hasName tag
tagData = getChildren    >>> getText
downFrom top a = proc e -> do
                   returnA <<< hasName top /> a -< e
extractFrom mk top a  = arr mk <<< tagData <<< downFrom top a
extractFromEntry mk = extractFrom mk "entry"


entry = atTag "entry" >>>
        proc e -> do
          acc   <- single accession    -< e
          ename <- entryName           -< e
          p     <- proteinName         -< e
          oname <- single organismName -< e
          returnA -< Entry {
                        _accession = acc
                      , _entry     = ename
                      , _protein   = p
                      , _organism  = oname }
    

accession = extractFromEntry id $ hasName "accession"
entryName = extractFromEntry id $ hasName "name"
proteinName = proc entry -> do
                e <- single fullNames -< entry
                returnA -< e

fullNames = hasName "entry"     /> hasName "protein" >>> atTag "fullName" >>> tagData

organismName = atTag "organism" /> hasName "name"    >>> tagData


mkInteraction :: [String] -> IO Interaction
mkInteraction [ori,tar,prob] =
    let ori' = uniprot ori
        tar' = uniprot tar
        res = parMap rwhnf (\xml -> runX (readDocument [] xml >>> entry)) [ori', tar']
    in (rearrange . flip mkInteraction' (read prob)) <$> sequence (res `seq` res)

        where uniprot id = "http://www.uniprot.org/uniprot/" ++ id ++ ".xml"
              
              rearrange :: Interaction -> Interaction
              rearrange i = if has _organism "Plasmodium" (origin i)
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
          same s i = hasBoth _organism s (origin i) (target i)

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
