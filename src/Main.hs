{-# LANGUAGE ImplicitParams #-}
module Main where
import System.FilePath
import System.Directory
import Control.Monad
import Control.Exception
import System.Environment
import System.Console.GetOpt
import System.IO
import qualified Data.Map as M
import Data.List
import Data.IORef
import CHPath.Paths
import CHPath.AST
import Debug.Trace

debugShow :: Show a => String -> a -> a
--debugShow m a = trace (((showString m) . (showString " ")) (show a)) a
debugShow _ a = a

type ChangeOpt = OptionData -> OptionData

options :: [OptDescr ChangeOpt]
options = [Option ['r'] ["remove"] (NoArg choptRemove) "remove all occurrences",
           Option ['s'] ["script"] (NoArg choptScript) "create a temporary script file"]

-- | Holds all the options and data that represent a chpath command
data OptionData = OptionData {
      optRemove :: Bool,            -- ^ Whether we are adding or removing (-r option)
      optScript :: Bool             -- ^ Whether to write a shell script file
} deriving Show

emptyOptionData = OptionData { optRemove=False,
                               optScript=False}

choptRemove o = setOptRemove True o
choptScript o = setOptScript True o

setOptRemove b opt = opt { optRemove = b }
setOptScript b opt = opt { optScript = b }

getDirDefs :: (?envref :: EnvRef, ?dirref :: DirRef) => IO ()
getDirDefs = do
  let etcdefs = "/etc" </> "chpathrc"
  etcexists <- doesFileExist etcdefs
  when etcexists $ parseDefaults "/" etcdefs
  home <- getHomeDirectory
  let homedefs = home </> ".chpathrc"
  exists <- doesFileExist homedefs
  when exists $ parseDefaults home homedefs
  return ()

main = do
  argv <- getArgs
  case getOpt Permute options argv of
        (options,args,[]) -> do
          envref <- newIORef M.empty
          dirref <- newIORef M.empty
          let ?envref = envref
          let ?dirref = dirref
          let opts = foldl (flip id) emptyOptionData options
          nh <- if optScript opts then openTempPosix else return $ nhFromH stdout
          finally
            (do
              getDirDefs
              cwd <- getCurrentDirectory
              let statements = parseCLIStatements (unwords args)
              performOnList (optRemove opts) cwd statements
              nhPutPosixScript nh)
            (do
              nhPutPosixRm nh
              hClose (nhHandle nh)
              nhPrintName stdout nh)

        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: [options] path"
