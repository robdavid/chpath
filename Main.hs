module Main where
import Data.List.Utils
import System.FilePath
import System.Directory
import Control.Monad
import System.Environment
import System.Console.GetOpt

data Option = AddZ String
            | AddA String
            | Skip String
            | Remove String

rPath :: FilePath -> [FilePath] -> IO [FilePath]
rPath _ [] = return []
rPath f (d:ds) = do
      cf <- canonicalizePath f
      cp <- canonicalizePath d
      if cf `equalFilePath` cp then mds else (d:) `liftM` mds
         where mds = rPath f ds 
               

addAAfter :: [FilePath] -> FilePath -> [FilePath] -> [FilePath]
addAAfter after f path = prefix ++ [f] ++ suffix
          where (rsuffix,rprefix) = break ((flip elem) after) (reverse path) 
                suffix = reverse rsuffix
                prefix = reverse rprefix

addZBefore :: [FilePath] -> FilePath -> [FilePath] -> [FilePath]
addZBefore before f path = prefix ++ [f] ++ suffix
    where (prefix,suffix) = break ((flip elem) before) path

pathJoin :: [FilePath] -> String
pathJoin [] = ""
pathJoin p =
    case p of
      [x] -> maybeQuote x 
      (x:xs) -> (maybeQuote x) ++ (':' : pathJoin xs)
    where maybeQuote x = if ' ' `elem` x then "\"" ++ x ++ "\"" else x
               
processOpts :: ([FilePath],[FilePath]) -> Option -> IO ([FilePath],[FilePath])
processOpts (a,p) (Skip s) = return ((s:a),p)
processOpts (a,p) (AddA d) = do
  p' <- rPath d p
  if a==[] then return (a,(d:p')) else return (a,addAAfter a d p')
processOpts (a,p) (AddZ d) = do
  p' <- rPath d p
  if a==[] then return (a,p'++[d]) else return (a,addZBefore a d p')
processOpts (a,p) (Remove d) = ((,) a) `liftM` rPath d p

options :: [OptDescr Option]
options = [Option ['a'] ["add-start"] (ReqArg AddA "DIR") "add directory to start",
           Option ['z'] ["add-end"] (ReqArg AddZ "DIR") "add directory to end",
           Option ['s'] ["skip"] (ReqArg Skip "DIR") 
                      "skip (preserve) any occurrences at start or end",
           Option ['r'] ["remove"] (ReqArg Remove "DIR") "remove all occurrences" ]


main = do
  argv <- getArgs
  case getOpt Permute options argv of
        (options,args,[]) -> do
            (a',path') <- case args of 
              [] -> foldM processOpts ([],[]) options
              [path] -> foldM processOpts ([], split ":" path) options
              _ -> error "Only one input path expected"
            putStrLn $ pathJoin path'
                                       
     
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: [options] path"