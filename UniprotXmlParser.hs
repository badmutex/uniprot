{-# LANGUAGE
  Arrows
  , EmptyDataDecls
  , NoMonomorphismRestriction
  #-}
module Main where

import Control.Parallel.Strategies
import Control.Applicative
import Text.XML.HXT.Arrow
import Text.Printf
import System.Directory
import System.Environment
import System.FilePath


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
          returnA -< (acc, ename, p)



accession = single <<< extractFromEntry id $ hasName "accession"
entryName = extractFromEntry id $ hasName "name"
proteinName = proc entry -> do
                e <- single fullNames -< entry
--                 e <- getName <<< getChildren <<< (hasName "entry" /> hasName "protein") -< entry
--                 s <- tagData  <<< deepest (hasName "fullName") -< e
                returnA -< e

fullNames = tagData <<< atTag "fullName" <<< hasName "entry" /> hasName "protein"
-- recommendedFullName = hasName "entry" /> hasName "protein" /> hasName "recommendedName" /> hasName "fullName"





showResult (acc_No, e_name, full_name) =
    printf "%s;%s;%s\n" acc_No e_name full_name

printResults rs = do
--   showResult ("Acc#", "Entry Name", "Full name")
  mapM_ showResult rs

printFullName (_, _, n) = putStrLn n


main = do
  accessionNos <- words <$> getContents
--   let accessionNos = ["P25098"]
  let xmls = parMap rwhnf (\no -> "http://www.uniprot.org/uniprot/" ++ no ++ ".xml") accessionNos
  ns <- concat `fmap` mapM (\xml -> runX (readDocument [] xml >>> entry)) xmls
  mapM_ showResult ns



