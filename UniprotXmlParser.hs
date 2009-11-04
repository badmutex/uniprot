{-# LANGUAGE
  Arrows
  , BangPatterns
  #-}
module Main where

import Control.Applicative
import Control.Monad (sequence)
import Control.Parallel.Strategies (parMap, rwhnf)
import Data.List (intercalate, isInfixOf)
import System.Environment
import Text.XML.HXT.Arrow
import System.IO.Unsafe


atTag tag = deep (isElem >>> hasName tag)
atTag' tag = isElem >>> hasName tag
tagData = getChildren >>> getText
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
          returnA -< [acc, ename, p, oname]
    

accession = extractFromEntry id $ hasName "accession"
entryName = extractFromEntry id $ hasName "name"
proteinName = proc entry -> do
                e <- single fullNames -< entry
                returnA -< e

-- fullNames = tagData <<< atTag "fullName" <<< hasName "entry" /> hasName "protein"
fullNames = hasName "entry" /> hasName "protein" >>> atTag "fullName" >>> tagData

-- organismName = tagData <<< atTag "organism" /> hasName "name"
organismName = atTag "organism" /> hasName "name" >>> tagData


uniprot id = "http://www.uniprot.org/uniprot/" ++ id ++ ".xml"

work :: [String] -> IO String
work [ori,tar,_,prob] = let ori' = uniprot ori
                            tar' = uniprot tar
                            !res = parMap rwhnf (\xml -> runX (readDocument [] xml >>> entry)) [ori', tar']
                        in ((++ ";" ++ prob) . intercalate ";" . concat . concat . rearrange) <$> sequence res
                            where rearrange s@[[a],[b]] = if "Plasmodium" `isInfixOf` (last a)
                                                          then [[notLast b],[notLast a]]
                                                          else [[notLast a],[notLast b]]
                                  notLast xs = take (length xs - 1) xs
main = do
  interactions <- (map words . lines) <$> getContents
  names        <- mapM work interactions
--   names <- runX (readDocument [] (uniprot p53) >>> entry)
  mapM_ putStrLn names
