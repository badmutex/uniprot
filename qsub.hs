module Main where

import Control.Applicative((<$>))
import Control.Parallel.Strategies
import Control.Monad
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Exit (ExitCode)
import System.FilePath ((</>), takeFileName)
import System.Process

import qualified Data.ByteString.Lazy.Char8 as BS

type Name = String
type Executable = String

user = "-M cabdulwa@nd.edu"

mkQsub file = "qsub " ++ user ++ " " ++ "-N " ++ (takeFileName file)

mkCmd  exec file = "cat " ++ file ++ " | " ++ exec ++ " 1>" ++ file ++ ".out" ++ " 2>" ++ file ++ ".err"

qsub :: Executable -> FilePath -> IO ExitCode
qsub exec file = system $
                "echo " ++ "'" ++ mkCmd exec file ++ "'" ++
                " | " ++
                mkQsub file

qsub' exec file = return $ 
                  "echo " ++ "'" ++ mkCmd exec file ++ "'" ++
                  " | " ++
                  mkQsub file

window :: Int -> [a] -> [[a]]
window _ [] = []
window s bs = take s bs : window s (drop s bs)

saveInfo :: (Integer,String) -> [BS.ByteString] -> IO (Integer, String)
saveInfo (i,path) bs = do BS.writeFile (path ++ show i) (BS.unlines bs)
                          return (i+1,path)

saveInfo' (p,bs) = BS.writeFile p (BS.unlines bs)

getFileNames :: Int -> String -> [String]
getFileNames s path = parMap rnf snd . take s $ iterate (\(i,p) -> (i+1, path ++ show i)) (1,path ++ "0")

main = do
  [exec,size,infile,outdir,prefix] <- getArgs
  let s = read size
  info <- (window s . BS.lines) <$> BS.readFile infile
  let files = getFileNames (length info) (outdir</>prefix)
  createDirectoryIfMissing True outdir
  mapM_ saveInfo' (files `zip` info)
  mapM_ (\f -> qsub exec f) files
