{-# LANGUAGE DeriveDataTypeable #-}
module CHPath.Paths (
                     RootedPath(..), PathEnvironment(..), AnalyzedAst(..), ChangePath, DirDefMap,
                     envToMap, rootOf, canonize,
                     normalizedPathList, analyzeAST, removePath,
                     getEnvAsPaths, parseFile,
                     parseCLIStatements, parseDefaults,
                     removeAll, insertPath, insertAll, removeInsertAll,
                     parseMap, openTempPosix, nhFromH, applyMapping,
                     nhPutPosixScript, nhPutPosixRm,
                     nhHandle, nhPrintName, rootPaths
)where
import System.Directory
import System.FilePath
import System.Environment
import System.IO
import Data.List
import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Control.Exception as EX
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import CHPath.Grammar
import CHPath.AST
import Control.Exception
import Data.Typeable
import Data.Maybe

-- | A qualified file path that may be relative to another path
data RootedPath = (QualifiedPath) :@: (Maybe FilePath) deriving Show

-- | An exception type that indicates a parsing error
data CHPathError = CHPParseError ParseError deriving Typeable

newtype PathEnvironment  a = PathEnvironment (M.Map String [a]) deriving (Show,Eq)

-- | A representation of an environment of simple file paths
type Environment = PathEnvironment FilePath

-- | A representation of an environment of rooted file paths
type RootedEnvironment = PathEnvironment RootedPath

-- | A change path specification. A change path specifies how an environment
-- can be transformed to a new environment
type ChangePath = PathEnvironment RootedPath

-- | A list of AnalyzedAst are generated from an AST created from parsing
-- a .chpath file. This is an intermediate step to creating a ChangePath instance
data AnalyzedAst = AAEnv (PathEnvironment QualifiedPath) |
                   AADir FilePath |
                   AADirDef FilePath [AnalyzedAst] deriving(Show,Eq)

type DirDefMap = M.Map String [AnalyzedAst]



-- | Converts a PathEnvironment to a Map of String to PathEnvironment's parametric type
envToMap (PathEnvironment m) = m

instance Show CHPathError where
    show (CHPParseError p) = "parse error in " ++ show p

instance Exception CHPathError

-- | A 'RootedPath' is an instance of 'Qualified', as it's file path is qualified (QualifiedFilePath)
instance Qualified RootedPath where
    qualifiersOf (qp :@: _) = qualifiersOf qp
    qualify qs (qp :@: r) = (qualify qs qp) :@: r

{-| A 'PathEnvironment' is a monoid; two or more may be combined to a form a
new PathEnvironment -}
instance Monoid (PathEnvironment a) where
    -- | Identity monoid is an empty map
    mempty =  PathEnvironment M.empty
    -- | Appends and merges keys/values from second map into first
    mappend (PathEnvironment p1) (PathEnvironment p2) = PathEnvironment $ M.foldWithKey (M.insertWith (flip (++))) p1 p2

-- | Extracts the base directory of a 'RootedPath'.
rootOf (_ :@: r) = r

-- | Extracts the 'Promote' qualifiers from a 'RootedPath' and forms a list
-- of rooted paths by combining each promote path with the base directory
-- of the rooted path. Thus the promote qualifiers are assumed to be rooted
-- in the same directory as the rooted path to which they belong.
rootedPromoted :: RootedPath -> [RootedPath]
rootedPromoted r = map (:@: rootOf r) (promoted r)

-- | Checks if a given file or directory exists.
doesExist file = (||) <$> doesFileExist file <*> doesDirectoryExist file

-- | Turns a 'RootedPath' into a canonical file path, combining the base directory
-- with the qualified path in a consistent way.
canonize :: RootedPath -> IO (Maybe FilePath)
canonize (p :@: _) | isLiteral p = return $ Just (filePathOf p)
canonize (p :@: Nothing) = do
  let fp = (filePathOf p)
  exists <- doesExist fp
  if exists
    then Just <$> canonicalizePath fp
    else return Nothing
canonize (p :@: Just r) = do
  let fp = r </> (filePathOf p)
  exists <- doesExist fp
  if exists
    then Just <$> canonicalizePath fp
    else return Nothing

-- | Typeclass that provides an equality function in the IO monad. Minimum implementation is
-- the (<==>) function.
class IOEq e where
    (<==>) :: e -> e -> IO Bool -- ^ Test for equality
    (</=>) :: e -> e -> IO Bool -- ^ Test for inequality
    (</=>) e1 e2 = not <$> (e1 <==> e2)

-- | 'RootedPath' is an instance of 'IOEq', which implements equality by comparing
-- cannonised expansions.
instance IOEq RootedPath where
    (<==>) p1 p2 = pure (==) <*> canonize p1 <*> canonize p2

-- | Transform an AST 'PathList' to a normalized, flattened list of qualified paths.
normalizedPathList :: [Qualifier] -> PathList -> [QualifiedPath]
normalizedPathList qs (PathList pes) = normalizeElems qs pes
    where normalizeElems _ [] = []
          normalizeElems qs (PathElem qs' (Path p) : pes) = ((QualifiedPath (qunion qs qs')  p) : (normalizeElems qs pes))
          normalizeElems qs ((PathElemList qs' pl) : pes) = (normalizedPathList (qunion qs qs') pl) ++ (normalizeElems qs pes)

-- | A variation of the prelude elem function in the IO monad that tests if a given element is in list, using the '(<==>)' function
-- from 'IOEq' to test for equaliy.
ioElem :: (IOEq a) => a -> [a] -> IO Bool
ioElem _ [] = return False
ioElem a (e:es) = (pure (||)) <*> (a <==> e) <*> ioElem a es

-- | Removes all occurances of an element from a list of elements,
-- where all elements are instaces of 'IOEq' and '(<==>)' is used for
-- equality determination. Intended for removing specific
-- 'RootedPath's from a list of paths.
removePath :: (IOEq a) => a -> [a] -> IO [a]
removePath e = filterM (e </=>)

-- | Finds the value of a named environment variable, expressed a as a
-- search list of 'RootedPath's. The text of the variable is split
-- by search path character and mapped to a list of 'RootedPath's
-- without base directorties.
getEnvAsPaths :: String -> IO [RootedPath]
getEnvAsPaths env =
  EX.handle ((\_ -> return []):: IOError -> IO [RootedPath])
             (getEnv env >>= (return . map ((:@: Nothing) . QualifiedPath [Literal]) . splitSearchPath))

-- | Parse a ".chpath" file, given a file path. This can be the name of a file to parse
-- definitions from, or a directory that contains a .chpath file, which is parsed.
-- Successfully parsing yields a list of 'Statement' values.
parseFile :: SourceName -> IO (Either ParseError [Statement])
parseFile fname = do
  { isDir <- doesDirectoryExist fname;
    if isDir then return $ fname </> ".chpath" else return fname } >>= parseFromFile definitions

parseDefaultsFile :: SourceName -> IO (Either ParseError [Statement])
parseDefaultsFile = parseFromFile rcDefinitions 
  

-- | Uses 'removePath' to remove several paths from a list of paths.
removeAll :: [RootedPath] -- ^ A list of paths to be removed
             -> [RootedPath] -- ^ A list of path from which paths will be removed
             -> IO [RootedPath] -- ^ Result containing remaining paths, in IO Monad
removeAll ips ops = foldM (flip removePath) ops ips

-- | Insert a path somewhere in a list of paths. Any replace, append or
-- promote qualifies of the path to be added are honoured.
insertPath :: RootedPath -> [RootedPath] -> IO [RootedPath]
insertPath ip ops = if isReplace ip
                    then return [ip]
                    else if isAppend ip
                    then do
                      (x,y) <- promoteSplitr (rootedPromoted ip) ops
                      return $ x ++ (ip:y)
                    else do
                      (x,y) <- promoteSplitl (rootedPromoted ip) ops
                      return $ x ++ (ip:y)
                    where promoteSplitl _ [] = return ([],[])
                          promoteSplitl prs (p:ps) = do
                            match <- ioElem p prs
                            (x,y) <- promoteSplitl prs ps
                            if match || not (null x)
                               then return ((p:x),y) else return ([],p:y)
                          promoteSplitr _ [] = return ([],[])
                          promoteSplitr prs (p:ps) = do
                            match <- ioElem p prs
                            (x,y) <- promoteSplitr prs ps
                            if match
                              then return ([],(p:x) ++ y) else return ((p:x),y)

-- | Uses 'insertPath' to insert a list of paths into a list of
-- paths. The order of the inserted paths is preserved, which means
-- that prepended paths are inserted in reverse order
insertAll :: [RootedPath] -- ^ The list of paths to be inserted
             -> [RootedPath] -- ^ The list of paths to add to
             -> IO [RootedPath]
insertAll ips ops = foldM (flip insertPath) ops insList
    where insList = (reverse preps) ++ apps
          (preps,apps) = partition isPrepend ips

-- | Main function for adding new paths. Uses 'removalAll' and to first
-- strip out any paths that are duplicates of the paths to be
-- inserted and then 'insertAll' to insert the new paths
removeInsertAll :: [RootedPath] -> [RootedPath] -> IO [RootedPath]
removeInsertAll ips ops = removeAll ips ops >>= insertAll ips

-- | Produces a simple AST from a list of parsed statements from a
-- .chpath file.
analyzeAST :: [Statement] -> [AnalyzedAst]
analyzeAST nodes = reverse (cons lastEnv finalAAs)
                 where

                   (_,finalAAs,lastEnv) = foldl pf ([],[],PathEnvironment M.empty) nodes

                   pf :: ([Qualifier], [AnalyzedAst], PathEnvironment QualifiedPath) ->
                         Statement ->
                         ([Qualifier], [AnalyzedAst], PathEnvironment QualifiedPath)
                   pf (_,aas,m) (QualifierStatement qs) = (qs,aas,m)
                   pf (qs,aas,PathEnvironment e) (AssignmentStatement (Assignment na ps)) = (qs,aas,(PathEnvironment e'))
                       where e' = M.insertWith (flip (++)) na (normalizedPathList qs ps) e
                   pf (qs,aas,e) (DirectoryStatement (Path d)) = (qs, (AADir d) : (cons e aas), PathEnvironment M.empty)
                   pf (qs,aas,e) (DirectoryDefStatement (DirectoryDef (Path d) stms)) =
                       (qs,(AADirDef d (analyzeAST stms)) : aas,e)

                   cons (PathEnvironment e) aas | M.null e = aas
                   cons pe aas = (AAEnv pe : aas)

defDirAST :: Maybe FilePath -> FilePath -> [AnalyzedAst] -> Maybe [AnalyzedAst]
defDirAST d f asts = let dirdef = find matchdir asts
                         getasts (AADirDef _ as) = as
                         matchdir (AADirDef dd _) = case d of
                                                      Just d' -> (d'</>dd) `equalFilePath` f
                                                      Nothing -> dd `equalFilePath` f
                         matchdir _ = False
                     in getasts <$> dirdef

mapDefDirAST :: Maybe FilePath -> [AnalyzedAst] -> IO DirDefMap
mapDefDirAST d asts = (M.fromList.catMaybes) <$> sequence (map findDefs asts)
                      where findDefs (AADirDef dd aas) = case d of
                              Just d' -> do
                                path <- canonicalizePath (d'</>dd)
                                return $ Just (path, aas)
                              Nothing -> return $ Just (dd, aas)
                            findDefs _ = return Nothing

-- | Produces a 'ChangePath' map (which can transform an environment)
-- from an AST and a directory location relative to which the AST is
-- interpreted.
rootPaths ::  DirDefMap -> FilePath -> [AnalyzedAst] -> IO ChangePath
rootPaths defmap dir aas = (mconcat.catMaybes) <$> (sequence (map rootAA aas))
                    where rootAA :: AnalyzedAst -> IO (Maybe ChangePath)
                          rootAA (AAEnv (PathEnvironment env)) =
                              return $ Just $ PathEnvironment $ M.map (map (:@: Just dir)) env
                          rootAA (AADir d) = do
                            defmap' <- mapDefDirAST (Just dir) aas
                            Just <$> parseMap (M.union defmap' defmap) (dir</>d)
                          rootAA _ = return Nothing


-- | Parses a defaults (rc) file
parseDefaults :: FilePath -> IO DirDefMap
parseDefaults f = do
  exists <- doesFileExist f
  if exists 
        then do
            file <- canonicalizePath f
            let dir = takeDirectory file
            parseResult <- parseDefaultsFile file
            case parseResult of
              Left err -> throwIO $ CHPParseError err
              Right nodes -> mapDefDirAST (Just dir) (analyzeAST nodes)
        else 
            fail $ "File " ++ f ++ " does not exist"


-- | Looks for a .chpath file in the given directory, and if found
-- produces a 'ChangePath' map (which can transform an environment)
-- for paths relative to that directory.
parseMap :: DirDefMap -> FilePath -> IO ChangePath
parseMap defmap dir = do
  dexists <- doesDirectoryExist dir
  if dexists 
        then do
               cdir <- canonicalizePath dir
               let maas = M.lookup cdir defmap
               let chfile = cdir </> ".chpath"
               fexists <- doesFileExist chfile
               if fexists 
                then do
                    parseResult <- parseFile chfile
                    case parseResult of
                      Left err -> throwIO $ CHPParseError err
                      Right nodes -> rootPaths defmap cdir $ concat (maybeToList maas) ++ analyzeAST nodes
                else case maas of
                   Just aas -> rootPaths defmap cdir aas
                   Nothing -> fail $ "File " ++ chfile ++ " does not exist"
        else
           fail $ "Directory " ++ dir ++" does not exist"


-- | Parse a list of CLI statements with is either a path assignment
-- statement or just the right hand side of an assignment.
parseCLIStatements :: String -> [Statement]
parseCLIStatements stmt = case (parse statementList stmt stmt) of
                               Left err -> throw $ CHPParseError err
                               Right statements -> statements

-- | Applies 'parseMap' over a list of directory names and combines
-- the resultant 'ChangePath's into a single 'ChangePath' (using mconcat
-- because ChangePath is a Monoid).
--mapDirs :: [FilePath] -> IO ChangePath
--mapDirs fps = mconcat <$> sequence (map (parseMap Nothing) fps)

-- | Applies a transformation to the system environment. The
-- transformation is characterised by a function that combines two
-- lists of paths in some fashion (usually 'addAll' or 'removeAll'),
-- and a 'ChangePath' map of names to a list of paths, which are
-- combined with paths in the system environment using the function
applyMapping :: ([RootedPath] -> [RootedPath] -> IO [RootedPath])  -- ^ Function to combine path lists
                -> ChangePath -- ^ The mappings used to transform the system environment
                -> IO Environment -- ^ The transformed environment.
applyMapping tf mp = PathEnvironment <$> M.foldWithKey mergeEnv (return M.empty) (envToMap mp)
    where mergeEnv :: String -> [RootedPath] -> (IO (M.Map String [FilePath])) -> (IO (M.Map String [FilePath]))
          mergeEnv key val ia = do
            envs <- getEnvAsPaths key
            val' <- tf val envs
            paths <- catMaybes <$> mapM canonize val'
            M.insert key paths <$> ia

-- | Prefix shell special characters (\, ', ",  ) with an escape character (\)
shellEscape :: String -> String
shellEscape = concat . map mapping
              where mapping '\\' = "\\\\"
                    mapping '\'' = "\\'"
                    mapping ' ' = "\\ "
                    mapping '\"' = "\\\""
                    mapping x = [x]

-- | Joins a list of paths into a single search path string. Inverse of System.FilePath.splitSearchPath
joinSearchPath :: [FilePath] -> String
joinSearchPath ps = intercalate [searchPathSeparator] ps

-- | Writes a line to a shell script that sets (or possibly deletes) a search
-- environment variable.
-- @writePath h v ps@ will write a line to file handle @h@ with sets the variable named @v@ to
-- the search path represented by @ps@. If @ps@ is empty, then the script line @unset@s the variable.
writePath :: Handle -> String -> [FilePath] -> IO ()
writePath h e [] = hPutStrLn h $ "unset " ++ e
writePath h e ps = hPutStrLn h $ e ++ "=" ++ shellEscape (joinSearchPath ps)

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
nhPutPosixScript :: NamedHandle -> Environment -> IO ()
nhPutPosixScript nh@(_,h) mp =
  forM_ (M.toList (envToMap mp)) (\(k,v) -> writePath h k v) >>
  writeExports h  (M.keys (envToMap mp))

-- | Open a temporary shell script output file, and return a 'NamedHandle'.
openTempPosix :: IO NamedHandle
openTempPosix = nhFromPair <$> openTempFile "/tmp" "chpath"

-- | Print the file name of a named handle to an output stream.
nhPrintName :: Handle -> NamedHandle -> IO ()
nhPrintName h (Just f,_) = hPutStrLn h f
nhPrintName _ _ = return ()

