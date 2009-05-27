{-# LANGUAGE BangPatterns #-}
module Main where
import System.FilePath
import System.Directory
import Control.Monad
import Control.Applicative
import System.Environment
import System.Console.GetOpt
import System.IO
import qualified Data.Map as M
import Data.List
import Data.Monoid
import CHPath.Paths
import CHPath.AST
import Debug.Trace


debugShow :: Show a => String -> a -> a
--debugShow m a = trace (((showString m) . (showString " ")) (show a)) a
debugShow _ a = a

type ChangeOpt = OptionData -> OptionData

options :: [OptDescr ChangeOpt]
options = [Option ['a'] ["add-start"] (NoArg choptAddA) "add entries to start",
           Option ['z'] ["add-end"] (NoArg choptAddZ) "add entries to end",
           Option ['p'] ["promote"] (ReqArg choptPromote "DIR")
                      "promote any occurrences above new entries at start or end",
           Option ['r'] ["remove"] (NoArg choptRemove) "remove all occurrences",
           Option ['d'] ["directory"] (ReqArg choptDir "DIR") "a chpath managed directory",
           Option ['s'] ["script"] (NoArg choptScript) "create a temporary script file"]

-- | Holds all the options and data that represent a chpath command
data OptionData = OptionData {
      optStatements :: [Statement], -- ^ Accumulation of statements from the command line
      optQuals :: [Qualifier],      -- ^ Accumulation of qualifiers from the command line
      optRemove :: Bool,            -- ^ Whether we are adding or removing (-r option)
      optScript :: Bool,            -- ^ Whether to write a shell script file
      optDirDefs :: M.Map FilePath [AnalyzedAst] -- ^ Directory definitions
} deriving Show

emptyOptionData = OptionData { optStatements=[],
                               optQuals=[],
                               optRemove=False,
                               optScript=False,
                               optDirDefs=M.empty}

choptAddZ o = debugShow "opt z" (addOptQual [Append] o)
choptAddA o = debugShow "opt a"  (addOptQual [Prepend] o)
choptPromote d o = debugShow "opt p" (addOptQual [Promote (QualifiedPath [Literal] d)] o)
choptStatement s o = debugShow "statements"
                                             (addStatements (parseCLIStatements (debugShow "Parsing statements" s)) o)
choptRemove o = setOptRemove True o
choptScript o = setOptScript True o
choptDir d o = debugShow "opt d" (addStatements [DirectoryStatement (Path d)] o)

-- | Applies a function to the Statement list
addStatements ss opt@OptionData { optStatements = oss, optQuals=qs } =
    opt { optStatements = debugShow "new statements"
                          (oss ++ (if null qs then ss else ((QualifierStatement qs):ss))),
          optQuals=[] }
-- | Add a qualifier statement
addOptQual q opt@OptionData { optQuals = qs} = opt { optQuals = (qunion qs q) }
setOptRemove b opt = opt { optRemove = b }
setOptScript b opt = opt { optScript = b }

rollupStatements :: OptionData -> IO ChangePath
rollupStatements opt@OptionData {optStatements = ss, optDirDefs = ddefs} = do
  cwd <- getCurrentDirectory
  rootPaths ddefs cwd (debugShow "AST rollup" (analyzeAST (debugShow "statement rollup" ss)))

getDirDefs :: IO (M.Map FilePath [AnalyzedAst])
getDirDefs = do
  let etcdefs = "/etc" </> "chpathrc"
  etcexists <- doesFileExist etcdefs
  etcmap <- if etcexists 
            then parseDefaults etcdefs 
            else return M.empty
  home <- getHomeDirectory
  let homedefs = home </> ".chpathrc"
  exists <- doesFileExist homedefs
  homemap <- if exists 
             then parseDefaults homedefs 
             else return M.empty
  return $ homemap `M.union` etcmap

main = do
  argv <- getArgs
  case getOpt (ReturnInOrder choptStatement) options argv of
        (options,args,[]) -> do
          dirDefs <- getDirDefs
          let optData = emptyOptionData { optDirDefs = debugShow "rc defs" dirDefs }
          let opts = foldl (flip id) optData options
          chPath <- rollupStatements opts
          nh <- if optScript opts then openTempPosix else return $ nhFromH stdout
          let updateFun = if optRemove opts then removeAll else removeInsertAll
          applyMapping updateFun chPath >>= nhPutPosixScript nh
          nhPutPosixRm nh
          hClose (nhHandle nh)
          nhPrintName stdout nh


        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: [options] path"
