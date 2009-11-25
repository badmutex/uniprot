-- | Given a list of uniprot entries, download their XML files to a specified directory.
--
-- > cat uniprot.list | runhaskell GetXmls.hs ~/uniprot
--

module Main where

import Control.Applicative   ((<$>))
import Data.Either           (partitionEithers)
import Network.Curl.Download (openURI)
import System.Directory      (doesFileExist)
import System.Environment    (getArgs)
import System.FilePath       ((</>), (<.>))

import qualified Data.ByteString as BS

type CanFail = Either String
type UniprotAccessionNo = String

data Result = Result {
      file   :: FilePath                    -- ^ where to save the data
    , result :: CanFail BS.ByteString -- ^ either failure or file contents
    } deriving Show

downloadTo :: FilePath -> UniprotAccessionNo -> IO (CanFail Result)
downloadTo dir id = do
  let f         = dir </> id <.> "xml"
      uniprot s = "http://www.uniprot.org/uniprot/" ++ s ++ ".xml"

  fe <- doesFileExist f
  if fe
    then return . Left 
             $ f ++ " exists"
    else openURI (uniprot id) 
             >>= \res -> return . Right $ 
                      Result {
                       file   = f
                     , result = res}

saveResult :: FilePath -> CanFail BS.ByteString -> IO ()
saveResult _ (Left msg) = putStrLn msg
saveResult f (Right bs) = if BS.null bs -- some uniprot entries have been deleted yet
                                        -- they still show up as 'Right' for some reason
                          then putStrLn $ f ++ " was empty"
                          else putStrLn ("Wrote " ++ f) >>
                               BS.writeFile f bs

main = do
  [outdir]                  <- getArgs
  ids                       <- words <$> getContents
  xmls                      <- mapM (downloadTo outdir) ids
  let (existing,downloaded) =  partitionEithers xmls
  mapM_ putStrLn existing
  mapM_ (\r -> saveResult (file r) (result r)) downloaded
