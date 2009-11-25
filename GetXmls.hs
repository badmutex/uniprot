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
import System.FilePath       ((</>), (<.>), dropExtension, takeFileName)

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

fromRight (Right r) = r
fromRight (Left _) = error "fromRight: passed a Left"

saveResult :: CanFail Result -> IO () -- CanFail BS.ByteString -> IO ()
saveResult (Left msg)  = putStrLn msg
saveResult (Right (Result {file=path, result=contents})) =
    case contents of
      Left msg -> putStrLn msg
      _        -> let bs = fromRight contents
                      name = dropExtension . takeFileName $ path

                  -- some uniprot entries have been deleted yet
                  --  still show up as 'Right'
                  in if BS.null bs
                     then putStrLn $ name ++ " was empty"

                     else do putStrLn $ "Writing " ++ path
                             BS.writeFile path bs


main = do
  [outdir]                  <- getArgs
  ids                       <- words <$> getContents
  xmls                      <- mapM (downloadTo outdir) ids
  mapM_ saveResult xmls
