{-# LANGUAGE
  Arrows
  #-}
module Main where

import Control.Applicative
import Control.Monad (sequence)
import Control.Parallel.Strategies (parMap, rwhnf)
import Data.List (intercalate)
import System.Environment
import Text.XML.HXT.Arrow


atTag tag = deep (isElem >>> hasName tag)
atTag' tag = isElem >>> hasName tag
tagData = getChildren >>> getText
downFrom top a = proc e -> do
                   returnA <<< hasName top /> a -< e
extractFrom mk top a  = arr mk <<< tagData <<< downFrom top a
extractFromEntry mk = extractFrom mk "entry"


entry = atTag "entry" >>>
        proc e -> do
          acc   <- accession   -< e
          ename <- entryName   -< e
          p     <- proteinName -< e
          returnA -< [acc, ename, p]


accession = single <<< extractFromEntry id $ hasName "accession"
entryName = extractFromEntry id $ hasName "name"
proteinName = proc entry -> do
                e <- single fullNames -< entry
                returnA -< e

fullNames = tagData <<< atTag "fullName" <<< hasName "entry" /> hasName "protein"


uniprot id = "http://www.uniprot.org/uniprot/" ++ id ++ ".xml"

work :: [String] -> IO String
work [ori,tar,_,prob] = let ori' = uniprot ori
                            tar' = uniprot tar
                            !res = parMap rwhnf (\xml -> runX (readDocument [] xml >>> entry)) [ori', tar']
                        in ((++ ";" ++ prob) . intercalate ";" . concat . concat) <$> sequence res

main = do
  interactions <- (map words . lines) <$> getContents
  names        <- mapM work interactions
  mapM_ putStrLn names


