module Main where

import Control.Applicative((<$>))
import Control.Parallel.Strategies
import Control.Monad
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Process

import qualified Data.ByteString.Lazy.Char8 as BS

user = "-M cabdulwa@nd.edu"
mail = "-m bea"
-- parallel_env = "-pe omp 6"

mkQsub = "qsub " ++ user ++ " " ++ mail -- ++ " " ++ parallel_env

mkCmd exec file = "cat " ++ file ++ " | " ++ exec ++ " 1>" ++ file ++ ".out" ++ " 2>" ++ file ++ ".err"

--qsub :: String -> String -> IO ExitCode
qsub exec file = system $
                "echo " ++ "'" ++ mkCmd exec file ++ "'" ++
                " | " ++
                mkQsub

qsub' exec file = return $ 
                  "echo " ++ "'" ++ mkCmd exec file ++ "'" ++
                  " | " ++
                  mkQsub                  

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
  mapM (\f -> qsub exec f) files
