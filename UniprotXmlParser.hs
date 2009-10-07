{-# LANGUAGE
  Arrows
  , NoMonomorphismRestriction
  #-}
module Main where

import Text.XML.HXT.Arrow
import System.Directory
import System.FilePath

data Protein = Prot {
      recommendedName :: String
    } deriving (Eq, Show)

data Gene = Gene {
      geneType :: String
    , geneName :: String
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
          g     <- gene        -< e
          o     <- organism    -< e
          returnA -< (acc, o)
          -- returnA -< (acc, ename, p, g)


accession = extractFromEntry id $ hasName "accession"
entryName = extractFromEntry id $ hasName "name"
proteinName = extractFromEntry Prot $
              hasName "protein"         />
              hasName "recommendedName" />
              hasName "fullName"

gene = n &&& t >>> arr (uncurry mkGene)
    where n = extractFromEntry id $ hasName "gene" /> hasName "name"
          t = getAttrValue "type" <<< downFrom "entry" (hasName "gene" /> hasName "name")

organismNameType = getAttrValue "type" &&& getText >>> arr (uncurry mkOrganismName)

dbReference = getAttrValue "type" &&& getAttrValue "key" &&& getAttrValue "id" >>>
              arr (\(t, (k,i)) -> mkDBRef t k i)

organism = hasName "entry" /> hasName "organism" >>> multi organismNameType


main = do
  let xml = "Research/uniprot.git/uniprot_sprot.xml.top5"
  h <- getHomeDirectory
  ds <- runX (readDocument [] (h </> xml) >>> entry)
  mapM_ print ds



