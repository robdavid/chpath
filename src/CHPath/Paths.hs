{-# LANGUAGE ImplicitParams, DeriveDataTypeable #-}
module CHPath.Paths (
  simplify, unassign, evaluate, perform, performOnList, writeEnvs,
  parseCLIStatements,parseDefaults, openTempPosix, nhFromH,
  nhPutPosixScript, nhPutPosixRm, nhHandle, nhPrintName,
  EnvRef,DirRef
)where
import System.Directory hiding (makeAbsolute)
import System.FilePath
import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.IORef
import Control.Applicative
import Control.Monad
import qualified Control.Exception as EX
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import CHPath.Grammar
import CHPath.AST
import Data.Typeable
import Data.Maybe
import Text.ShellEscape (bash,bytes)
import Data.ByteString.Char8(pack,unpack)

simplify :: PathElement -> PathElement
simplify (PathList xs) = case nub $ foldr foldElements [] xs of
  [y] -> y
  ys -> PathList ys

  where foldElements ys b = case simplify ys of
                              PathList zs -> zs ++ b
                              e -> e:b

simplify (Subtract a b) = case Subtract (simplify a) (deopt (simplify b)) of
  Subtract (Optional a') b' -> Optional (simplify (Subtract a' b'))
  Subtract (PathList []) _ -> PathList []
  Subtract a' (PathList []) -> a'
  s -> s
  where deopt (Optional e) = e
        deopt e = e

simplify (Optional e) = case Optional (simplify e) of
  Optional o@(Optional e') -> o
  Optional (PathList []) -> PathList []
  o -> o

simplify x = x

type EnvRef = IORef (M.Map String String)
type DirRef = IORef (M.Map String [Statement])

lookupEnvRef :: (?envref :: EnvRef) => String -> IO (Maybe String)
lookupEnvRef name = do
  m <- readIORef ?envref
  let val = M.lookup name m
  case val of
    Nothing -> lookupEnv name
    Just _ -> return val

setEnvRef :: (?envref :: EnvRef) => String -> String -> IO ()
setEnvRef name value = do
  m <- readIORef ?envref
  let m' = M.insert name value m
  writeIORef ?envref m'

collapse :: FilePath -> FilePath
collapse = dropTrailingPathSeparator . joinPath . fixup . splitPath
  where fixup p = reverse $ foldl dd [] p
        dd l d | d == "." || d == "./" = l
        dd l d2 | d2 == ".." || d2 == "../" = tail l
        dd l x = x:l
makeAbsolute :: FilePath -> FilePath -> FilePath
makeAbsolute cwd fp =
  let absFp = if isAbsolute fp then fp else joinPath [cwd,fp] in collapse absFp

{-
-- | Create an unassignment expression, that revereses the additions, to a named environment variable, of a given expression
-}
unassign :: String -> PathElement -> PathElement
unassign envName element = Subtract (EnvVar envName) (simplify $ envRemove envName element)
  where envRemove :: String -> PathElement -> PathElement
        envRemove ename (EnvVar e) | ename == e = PathList []
        envRemove _ (Optional _) = PathList []
        envRemove ename (PathList pes) = PathList $ map (envRemove ename) pes
        envRemove ename (Subtract a b) = Subtract (envRemove ename a) b
        envRemove _ x = x

asList (PathList l) = l
asList x = [x]

evaluate :: (?envref :: EnvRef) => FilePath -> PathElement -> IO PathElement
evaluate cwd (PathList l) = pnub . deopt . simplify . PathList . lnub <$> mapM (evaluate cwd) l
  where lnub es = map merge es
          where merge (PathList el) = PathList (el \\ es)
                merge x = x
        deopt (PathList dl) = PathList $ foldr selopt [] dl
          where selopt (Optional x) l | x `elem` dl = x:l
                selopt (Optional _) l = l
                selopt x l | (Optional x) `elem` dl = l
                selopt x l = x:l
        deopt x = x
        pnub (PathList l) = PathList (nub l)
        pnub x = x
evaluate cwd (File f) = return $ Literal $ makeAbsolute cwd f
evaluate cwd (Optional e) = Optional <$> evaluate cwd e
evaluate cwd (Subtract a b) = do
  a' <- evaluate cwd a
  b' <- evaluate cwd b
  return (PathList (asList a' \\ asList b'))
evaluate _ (EnvVar name) = do
  env <- lookupEnvRef name
  case env of
    Just "" -> return $ PathList []
    Nothing -> return $ PathList []
    Just value -> return $ PathList $ map Literal (splitSearchPath value)
evaluate _ x = return x

asSearchPath :: PathElement -> String
asSearchPath (PathList l) = joinSearchPath (map asSearchPath l)
asSearchPath (Literal s) = s
asSearchPath x = show x
{-
asSearchPath (File s) = s
asSearchPath (Optional e) = asSearchPath e
asSearchPath (Subtract a b) = (asSearchPath a) \\ (asSearchPath b)
-}

-- | Parse a ".chpath" file, given a file path. This can be the name of a file to parse
-- definitions from, or a directory that contains a .chpath file, which is parsed.
-- Successfully parsing yields a list of 'Statement' values.
parseFile :: SourceName -> IO (Either ParseError [Statement])
parseFile fname = do
    isDir <- doesDirectoryExist fname;
    chpfile <- if isDir then do
        let fname1 = fname </> ".chpath"
        let fname2 = fname </> ".chpath2"
        isChp2 <- doesFileExist fname2
        if isChp2 then return fname2 else return fname1
      else return fname
    parseFromFile definitions chpfile

parseDefaultsFile :: SourceName -> IO (Either ParseError [Statement])
parseDefaultsFile = parseFromFile rcDefinitions

-- | Parses a defaults (rc) file
parseDefaults :: (?envref :: EnvRef, ?dirref :: DirRef) => FilePath -> FilePath -> IO ()
parseDefaults base f = do
  exists <- doesFileExist f
  if exists
        then do
            file <- canonicalizePath f
            let dir = takeDirectory file
            parseResult <- parseDefaultsFile file
            case parseResult of
              Left err -> EX.throwIO $ CHPParseError err
              Right statements -> performOnList False base statements
        else
            fail $ "File " ++ f ++ " does not exist"




-- | An exception type that indicates a parsing error
data CHPathError = CHPParseError ParseError deriving Typeable

instance Show CHPathError where
    show (CHPParseError p) = "parse error in " ++ show p

instance EX.Exception CHPathError


-- | Parse a list of CLI statements.
parseCLIStatements :: String -> [Statement]
parseCLIStatements stmt = case parse cliDefinitions stmt stmt of
                               Left err -> EX.throw $ CHPParseError err
                               Right statements -> statements


perform :: (?envref :: EnvRef, ?dirref :: DirRef) => Bool -> FilePath -> Statement -> IO ()
perform rev cwd (Assignment name pe unpe) = do
  let pe' = case (rev,unpe) of
              (False,_) -> simplify pe
              (True,Nothing) -> unassign name (simplify pe)
              (True,Just un) -> un
  --let revid (Just un) = const un
  --    revid Nothing = unassign name
  newlist <- evaluate cwd pe'
  setEnvRef name (asSearchPath newlist)
perform rev cwd (DirectoryDef dir statements) = do
  subdir <- canonicalizePath (cwd </> dir)
  let addStatements = M.insertWith (++) subdir statements
  modifyIORef ?dirref addStatements
perform rev cwd (Directory dir) = do
  subdir <- canonicalizePath (cwd </> dir)
  dirref <- readIORef ?dirref
  case M.lookup subdir dirref of
    Just statements -> performOnList rev subdir statements
    Nothing -> do
      parseResult <- parseFile subdir
      case parseResult of
        Left pe -> error $ show pe
        Right statements -> performOnList rev subdir statements

isDirectoryDef (DirectoryDef _ _) = True
isDirectoryDef _ = False

performOnList :: (?envref :: EnvRef, ?dirref :: DirRef) => Bool -> FilePath -> [Statement] -> IO ()
performOnList rev cwd statements = do
  let (dirdefs,otherStatements) = partition isDirectoryDef statements
  mapM_ (perform False cwd) dirdefs
  mapM_ (perform rev cwd) (if rev then reverse statements else statements)

-- | Prefix shell special characters (\, ', ",  ) with an escape character (\)
shellEscape :: String -> String
shellEscape = unpack . bytes . bash . pack

-- | Joins a list of paths into a single search path string. Inverse of System.FilePath.splitSearchPath
joinSearchPath :: [FilePath] -> String
joinSearchPath = intercalate [searchPathSeparator]

-- | Writes a line to a shell script that sets (or possibly deletes) a search
-- environment variable.
-- @writePath h v ps@ will write a line to file handle @h@ with sets the variable named @v@ to
-- the search path represented by @ps@. If @ps@ is empty, then the script line @unset@s the variable.
writeEnv :: Handle -> String -> String -> IO ()
writeEnv h e [] = hPutStrLn h $ "unset " ++ e
writeEnv h e ps = hPutStrLn h $ e ++ "=" ++ ps

writeEnvs :: (?envref :: EnvRef) => Handle -> IO ()
writeEnvs h = do
  envMap <- readIORef ?envref
  mapM_ (uncurry (writeEnv h)) (M.assocs envMap)
  writeExports h (M.keys envMap)


-- | Writes export statements to a shell script, given a file handle and a list of environment
-- variable names to export.
writeExports :: Handle -> [String] -> IO ()
writeExports h strs = write 80 [] strs
                      where write _ [] [] = return ()
                            write _ line [] = hPutStrLn h line
                            write n [] strs = write n "export" strs
                            write n line (str:strs) = if (length str) + (length line) + 1 > n
                                                      then hPutStrLn h line >> write n ("export " ++ str) strs
                                                      else write n (line ++ " " ++ str) strs

-- | A file handle with an optional file name
type NamedHandle = (Maybe FilePath, Handle)

-- | Make a 'NamedHandle' from a (name,handle) pair
nhFromPair (f,h) = (Just f, h)
-- | Make a 'NamedHandle' from a file handle
nhFromH h = (Nothing,h)

-- | Make a handle from a named handle
nhHandle :: NamedHandle -> Handle
nhHandle = snd

-- | Add a remove command to a POSIX shell script that removes itself.
nhPutPosixRm :: NamedHandle -> IO ()
nhPutPosixRm nh@(Just f,h) = hPutStrLn h ("rm -f " ++ f)
nhPutPosixRm nh = return ()

-- | Creates a POSIX shell script that modifies environment variables in
-- accordance to the environment provided. Export statements are written
-- for all variables modified.
nhPutPosixScript :: (?envref :: EnvRef ) => NamedHandle -> IO ()
nhPutPosixScript nh@(_,h) = writeEnvs h

-- | Open a temporary shell script output file, and return a 'NamedHandle'.
openTempPosix :: IO NamedHandle
openTempPosix = nhFromPair <$> openTempFile "/tmp" "chpath"

-- | Print the file name of a named handle to an output stream.
nhPrintName :: Handle -> NamedHandle -> IO ()
nhPrintName h (Just f,_) = hPutStrLn h f
nhPrintName _ _ = return ()
