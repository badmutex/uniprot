{-# LANGUAGE
  Arrows
  , EmptyDataDecls
  , NoMonomorphismRestriction
  #-}
module Main where

import Control.Applicative
import Text.XML.HXT.Arrow
import Text.Printf
import System.Directory
import System.Environment
import System.FilePath
import Network.Curl

data Protein

data ProteinName = Recommended | Submitted deriving (Eq, Show)

data Gene = Gene {
      geneType
    , geneName
      :: String
    } deriving (Eq, Show)

mkGene t n = Gene {geneType = t, geneName = n}

type Taxonomy = String

data Lineage = Lin {
      taxonomy :: [Taxonomy]
    } deriving (Eq, Show)


data OrganismName = Scientific String
                  | Common String
                  | Synonym String
                  | Unknown String
                    deriving (Eq, Show)

mkOrganismName "scientific" = Scientific
mkOrganismName "common" = Common
mkOrganismName "synonym" = Synonym
mkOrganismName _ = Unknown

data Organism = Org {
      orgNames :: [OrganismName]
    , orgLineage :: Lineage
    , orgDbRefs :: [DBRef]
    } deriving (Eq, Show)

type OrganismHost = Organism

data DBRef = DBRef {
      dbRefType :: String
    , dbRefKey :: String
    , dbRefId :: String
    } deriving (Eq, Show)

mkDBRef t k i = DBRef {dbRefType = t, dbRefKey = k, dbRefId = i}

data UniprotEntry = UEntry {
      accessionUE :: String
    , nameUE :: String
    , proteinUE :: Protein
    , geneUE :: Gene
    , organismUE :: Organism
    , dbreferencesUE :: [DBRef]
    }

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



accession = extractFromEntry id $ hasName "accession"
entryName = extractFromEntry id $ hasName "name"
proteinName = proc entry -> do
                e <- getChildren <<< getChildren <<< (hasName "entry" /> hasName "protein") -< entry

                s <- tagData  <<< deepest (hasName "fullName") -< e
                returnA -< s


gene = n &&& t >>> arr (uncurry mkGene)
    where n = extractFromEntry id $ hasName "gene" /> hasName "name"
          t = getAttrValue "type" <<< downFrom "entry" (hasName "gene" /> hasName "name")

organismNameType = getAttrValue "type" &&& getText >>> arr (uncurry mkOrganismName)

dbReference = getAttrValue "type" &&& getAttrValue "key" &&& getAttrValue "id" >>>
              arr (\(t, (k,i)) -> mkDBRef t k i)

organism = hasName "entry" /> hasName "organism" >>> multi organismNameType


showResult (acc_No, e_name, full_name) =
    printf "%-8s %-15s %s\n" acc_No e_name full_name

printResults rs = do
--   showResult ("Acc#", "Entry Name", "Full name")
  mapM_ showResult rs

printFullName (_, _, n) = putStrLn n


main = do
  accessionNos <- words <$> getContents
--   let accessionNos = ["P25098"]
  let xmls = map (\no -> "http://www.uniprot.org/uniprot/" ++ no ++ ".xml") accessionNos
  ns <- (map head) `fmap` mapM (\xml -> runX (readDocument [] xml >>> entry)) xmls
  printResults ns
--   mapM_ printFullName n



