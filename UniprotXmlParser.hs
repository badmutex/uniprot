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

data Organism = Org {
      scientificOrgName
    , commonOrgName
    , synonymOrgName :: String
    , orgLineage :: Lineage
    } deriving (Eq, Show)

data DBRef = DBRef {
      dbRefType :: String
    , dbRefKey :: String
    , dbRefId :: String
    } deriving (Eq, Show)

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
          returnA -< (acc, g)
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





main = do
  let xml = "Research/uniprot/uniprot_sprot.xml.top5"
  h <- getHomeDirectory
  ds <- runX (readDocument [] (h </> xml) >>> entry)
  mapM_ print ds



