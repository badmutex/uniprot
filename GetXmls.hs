module Main where

import Control.Applicative   ((<$>))
import Data.Either           (partitionEithers)
import Network.Curl.Download (openURI)
import System.Directory      (doesFileExist)
import System.Environment    (getArgs)
import System.FilePath       ((</>), (<.>))

import qualified Data.ByteString as BS



data Result = Result {
      file   :: FilePath
    , result :: Either String BS.ByteString
    } deriving Show

downloadTo :: FilePath -> String -> IO (Either String Result)
downloadTo dir id = do
  let f         = dir </> id <.> "xml"
      uniprot s = "http://www.uniprot.org/uniprot/" ++ s ++ ".xml"

  fe <- doesFileExist f
  if fe
    then return . Left 
             $ f ++ " exists."
    else openURI (uniprot id) 
             >>= \res -> return . Right $ 
                      Result {
                       file   = f
                     , result = res}

saveResult _ (Left msg) = putStrLn msg
saveResult f (Right bs) = BS.writeFile f bs

main = do
  [outdir]                  <- getArgs
  ids                       <- words <$> getContents
  xmls                      <- mapM (downloadTo outdir) ids
  let (existing,downloaded) =  partitionEithers xmls
  mapM_ putStrLn existing
  mapM_ (\r -> saveResult (file r) (result r)) downloaded
