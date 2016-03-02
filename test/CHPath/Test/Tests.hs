{-# LANGUAGE ImplicitParams #-}
module CHPath.Test.Tests (runAll, tests)
where

import CHPath.AST
import CHPath.Paths
import CHPath.Grammar
import Test.HUnit
import Data.IORef
import Data.List
import Data.Maybe
import Control.Applicative
import System.Directory
import System.FilePath
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S

import Text.ParserCombinators.Parsec

parsePathList :: String -> PathElement
parsePathList s =
    case runParser pathList () "" s of
      Left e -> error $ s ++ ": " ++ show e
      Right r -> r

parsePathListSimple = simplify . parsePathList

parseAndEvaluate cwd = (evaluate cwd) . simplify . parsePathList

parseStatement :: String -> Statement
parseStatement s =
    case runParser statement () "" s of
      Left e -> error $ s ++ ": " ++ show e
      Right r -> r

parseStatementList :: String -> [Statement]
parseStatementList s =
    case runParser statementList () "" s of
      Left e -> error $ s ++ ": " ++ show e
      Right r -> r

parseAndPerform rev cwd = (perform rev cwd) . parseStatement

runAll = runTestTT suite

tests::IO [Test]
tests = return [ suite ]

suite = test [
  "elemTest" ~: elemTest,
  "pathTest" ~: pathTest,
  "pathTerms" ~: pathTerms,
  "assignments" ~: assignments,
  "statements" ~: statements,
  "simplification" ~: simplification,
  "unassignment" ~: unassignment,
  "evaluation" ~: evaluation,
  "performAssignment" ~: performAssignment,
  "performUnassignment" ~: performUnassignment
  ]

elemTest = do
  parsePathList "a" @?= PathList [File "a"]
  parsePathList "[b]" @?= PathList [Literal "b"]
  parsePathList "[\"a b\"]" @?= PathList [Literal "a b"]
  parsePathList "@PATH" @?= PathList [EnvVar "PATH"]
  parsePathList "{[b]}" @?= PathList [Optional (Literal "b")]

pathTest = do
  parsePathList "a:b:c" @?= PathList [File "a",File "b", File "c"]
  parsePathList "d : e : f" @?= PathList [File "d",File "e", File "f"]
  parsePathList "x:@PATH:y" @?= PathList [File "x",EnvVar "PATH", File "y"]

pathTerms = do
  parsePathList "@PATH-a:b" @?= PathList [Subtract (EnvVar "PATH") (File "a"), File "b"]
  parsePathList "@PATH-(a:b)" @?= PathList [Subtract (EnvVar "PATH") (PathList [File "a", File "b"])]
  parsePathList "@PATH:(a:b)" @?= PathList [EnvVar "PATH", PathList [File "a", File "b"]]

assignments = do
  parseStatement "PATH += a" @?= Assignment "PATH" (PathList [EnvVar "PATH", File "a"]) Nothing
  parseStatement "PATH =+ b" @?= Assignment "PATH" (PathList [File "b", EnvVar "PATH"]) Nothing
  parseStatement "PATH -= c:d" @?= Assignment "PATH" (Subtract (EnvVar "PATH") (PathList [File "c", File "d"])) Nothing
  parseStatement "PATH -= c:d" @?= Assignment "PATH" (Subtract (EnvVar "PATH") (PathList [File "c", File "d"])) Nothing
  parseStatement "PATH = e:f" @?= Assignment "PATH" (PathList [File "e", File "f"]) Nothing
  parseStatement "PATH = @PATH - (e:f)" @?= Assignment "PATH" (PathList [Subtract (EnvVar "PATH") (PathList [File "e", File "f"])]) Nothing
  parseStatement "PATH = @PATH - g - h" @?= Assignment "PATH" (PathList [Subtract (Subtract (EnvVar "PATH") (File "g")) (File "h")]) Nothing
  parseStatement "PATH = (@PATH - i) - j" @?= Assignment "PATH" (PathList [Subtract (PathList [Subtract (EnvVar "PATH") (File "i")]) (File "j")]) Nothing
  parseStatement "PATH = " @?= Assignment "PATH" (PathList []) Nothing

statements = do
  parseStatementList "PATH += a; PATH =+ b\n"  @?=
    [Assignment "PATH" (PathList [EnvVar "PATH", File "a"]) Nothing,
    Assignment "PATH" (PathList [File "b", EnvVar "PATH"]) Nothing]
  parseStatementList "dirdef mydir { \n PATH += bin\n MYHOME=.\n };" @?=
    [DirectoryDef "mydir" [Assignment "PATH" (PathList [EnvVar "PATH",File "bin"]) Nothing,
                           Assignment "MYHOME" (PathList [File "."]) Nothing]]
  parseStatementList "PATH += a:b\n" @?=
    [Assignment "PATH" (PathList [EnvVar "PATH", File "a",File "b"]) Nothing]
  parseStatementList "PATH += \"a-a\":b\n" @?=
      [Assignment "PATH" (PathList [EnvVar "PATH", File "a-a",File "b"]) Nothing]
  parseStatementList "PATH += \"a-a\":\"b-b\"\n" @?=
    [Assignment "PATH" (PathList [EnvVar "PATH", File "a-a",File "b-b"]) Nothing]
  parseStatementList "PATH = @PATH:mybin ^ @OLDPATH" @?=
    [Assignment "PATH" (PathList [EnvVar "PATH", File "mybin"]) (Just (PathList [EnvVar "OLDPATH"]))]

simplification = do
  parsePathListSimple "@PATH:(a:b):c" @?= PathList [EnvVar "PATH", File "a", File "b", File "c"]
  parsePathListSimple "(@PATH:(a:b)):c" @?= PathList [EnvVar "PATH", File "a", File "b", File "c"]
  parsePathListSimple "(@PATH:(a:b)):c:a:b:@PATH" @?= PathList [EnvVar "PATH", File "a", File "b", File "c"]
  parsePathListSimple "@PATH:\"a-a\":b" @?= PathList [EnvVar "PATH", File "a-a", File "b"]
  parsePathListSimple "@PATH:d-()" @?= PathList [EnvVar "PATH", File "d"]
  parsePathListSimple "@PATH:d-{()}" @?= PathList [EnvVar "PATH", File "d"]
  parsePathListSimple "file" @?= File "file"
  parsePathListSimple "[name]" @?= Literal "name"
  parsePathListSimple "{{[name]}}" @?= Optional (Literal "name")
  parsePathListSimple "{{[name]}}:{()}" @?= Optional (Literal "name")
  parsePathListSimple "@PATH:{e}-f" @?= PathList [EnvVar "PATH", Optional (Subtract (File "e") (File "f"))]
  parsePathListSimple "@PATH:(g:{h}-i)" @?= PathList [EnvVar "PATH", File "g", Optional (Subtract (File "h") (File "i"))]
  parsePathListSimple "@PATH:(j:{k})-l" @?= PathList [EnvVar "PATH",Subtract (PathList [File "j",Optional (File "k")]) (File "l")]

unassignAssignment statement = case munpe of
                                Nothing -> unassign env pe
                                Just unpe -> unpe
  where Assignment env pe munpe = parseStatement statement

unassignment = do
  unassignAssignment "PATH += a:b:c" @?= Subtract (EnvVar "PATH") (PathList [File "a", File "b", File "c"])
  unassignAssignment "PATH = @PATH:(g:{h}-i)" @?= Subtract (EnvVar "PATH") (File "g")

evaluation = do
  cwd <- getCurrentDirectory
  paths <- nub . splitSearchPath <$> getEnv "PATH"
  let pathList = map Literal paths
  envref <- newIORef M.empty
  let ?envref = envref
  p0 <- parseAndEvaluate cwd "@PATH:a"
  p0 @?= PathList (pathList ++ [Literal (cwd </> "a")])
  p1 <- parseAndEvaluate cwd "@PATH:{a}"
  p1 @?= PathList pathList
  p2 <- parseAndEvaluate cwd ("@PATH:{" ++ head paths ++ "}")
  p2 @?= PathList ((tail pathList) ++ [head pathList])
  p3 <- parseAndEvaluate cwd "@NOPATH-a"
  p3 @?= PathList []

performAssignment = do
  cwd <- getCurrentDirectory
  let testPath = "/bin:/usr/bin:/usr/local/bin"
  envref <- newIORef (M.fromList [("PATH",testPath)])
  dirref <- newIORef M.empty
  let ?envref = envref
  let ?dirref = dirref
  p0 <- parseAndPerform False cwd "PATH=@PATH:a"
  envMap0 <- readIORef envref
  M.lookup "PATH" envMap0 @?= Just (testPath ++ ":" ++ (cwd </> "a"))
  p0 <- parseAndPerform False cwd "PATH+=[b]"
  envMap1 <- readIORef envref
  M.lookup "PATH" envMap1 @?= Just (testPath ++ ":" ++ (cwd </> "a") ++ ":b")
  p0 <- parseAndPerform False cwd "NOPATH-=b"
  envMap1 <- readIORef envref
  M.lookup "NOPATH" envMap1 @?= Just ""
  p0 <- parseAndPerform False cwd "NOPATH-=c"
  envMap1 <- readIORef envref
  M.lookup "NOPATH" envMap1 @?= Just ""

performUnassignment = do
  let cwd = "/usr/local"
  let testPath = "/bin:/usr/bin:/usr/local/bin"
  envref <- newIORef (M.fromList [("PATH",testPath)])
  dirref <- newIORef M.empty
  let ?envref = envref
  let ?dirref = dirref
  p0 <- parseAndPerform True cwd "PATH+=bin"
  envMap0 <- readIORef envref
  M.lookup "PATH" envMap0 @?= Just "/bin:/usr/bin"
{-

tests = test [ astTests, pathsTests ]

-- AST --
astTests = test [
         "checkQunion" ~: quickTest checkQunion,
         "checkQualify" ~: quickTest checkQualify
        ]

checkQunion (a,b) = nub c == c && c \\ (union a b) == []
    where c = qunion a b

checkQualify :: ([Qualifier],QualifiedPath) -> Bool
checkQualify (qs,n) = nub (qs `qunion` qualifiersOf n) == nub (qualifiersOf (qualify qs n))



--- Paths ---


pathsTests = test [
    "canonizeTest" ~: canonizeTest,
    "insertionPromoteTest" ~: insertionPromoteTest,
    "insertionDuplicateTest" ~: insertionDuplicateTest,
    "insertionOrderingTest" ~: insertionOrderingTest,
    "pathMapTests" ~: pathMapTests,
    "normalizationCheck" ~: quickTest normalizationCheck,
    "changePathMappendCheck" ~: quickTest changePathMappendCheck
    ]

canonizeTest = do
  fromJust <$> (canonize (QualifiedPath [] "/bin" :@: Nothing)) >>= (@?= "/bin")
  -- fromJust <$> (canonize (QualifiedPath [] "/bin" :@: Just "/usr")) >>= (@?= "/bin")
  fromJust <$> (canonize (QualifiedPath [] "bin" :@: Just "/usr")) >>= (@?= "/usr/bin")
  --fromJust <$> (canonize (QualifiedPath [Literal] "bin" :@: Just "/usr")) >>= (@?= "bin")
  fromJust <$> (canonize (QualifiedPath [] "." :@: Just "/usr")) >>= (@?= "/usr")

litPath x = QualifiedPath [Literal] x :@: Nothing

insertionPromoteTest = do
  let ipath = (
        QualifiedPath [Prepend,
                       Promote (QualifiedPath [Literal] "."),
                       Promote (QualifiedPath [Literal] "..")] "mypath" :@: Nothing
        )
  c1 <- insertPath ipath (map litPath [".", "p1", "..", "p2" ])
  (show c1) @?= (show [litPath ".",  litPath "p1", litPath "..", ipath, litPath "p2"])

insertionDuplicateTest = do
  let ipath = litPath "p1"
  c1 <- removeInsertAll [ipath] (map litPath ["p1","p2"])
  (show c1) @?= (show (map litPath ["p2", "p1"]))

insertionOrderingTest = do
  let ipaths = (map (qualify [Prepend] . litPath) ["p3", "p4"]) ++ (map litPath ["p1","p2"])
  c1 <- removeInsertAll ipaths (map (litPath . (showString "p") . show) [1..6])
  (show c1) @?= show (((map (qualify [Prepend] . litPath) ["p3","p4"])) ++ ((map litPath ["p5","p6","p1","p2"])))



normalizationCheck :: [Qualifier] -> PathList -> Bool
normalizationCheck qs pl =
  let qs' = qnub qs
      qps = normalizedPathList qs' pl in
  all (\qp -> qunion qs' (qualifiersOf qp) =$= qualifiersOf qp) qps

changePathMappendCheck :: ChangePath -> ChangePath -> Bool
changePathMappendCheck p1 p2 =
    let pm = mappend p1 p2
        pmk = M.keysSet (envToMap pm)
        p1k = M.keysSet (envToMap p1)
        p2k = M.keysSet (envToMap p2) in
    p1k `S.union` p2k == pmk

parseForTest :: String -> [Statement]
parseForTest s =
    case runParser definitions () "" s of
      Left e -> error $ s ++ ": " ++ (show e)
      Right r -> r

pathMapTests =
    let testf (x,y) = do { aas @?= y }
            where aas = analyzeAST (parseForTest x)
        mkEnv = AAEnv . PathEnvironment

    in
    mapM_ testf [
               ("a=b", [mkEnv (M.fromList[("a", [QualifiedPath [] "b"])])]),
               ("a={replace}b", [mkEnv (M.fromList[("a", [QualifiedPath [Replace] "b"])])]),
               ("directory \"/home/somewhere\" a={replace}b", [AADir "/home/somewhere",
                                                               mkEnv (M.fromList[("a", [QualifiedPath [Replace] "b"])])]),
               ("directory \"/home/somewhere\" a={replace}b include /home/somewhereelse",
                [AADir "/home/somewhere", mkEnv (M.fromList[("a", [QualifiedPath [Replace] "b"])]),
                 AADir "/home/somewhereelse"]),
               ("v1=p1 v2={append}p2", [mkEnv (M.fromList[("v1",[QualifiedPath [] "p1"]),("v2",[QualifiedPath [Append] "p2"])])]),
               ("v3={prepend}p1 v3={prepend}p2", [
                mkEnv (M.fromList [("v3", [QualifiedPath [Prepend] "p1",QualifiedPath [Prepend] "p2"] )])]),
               ("v4={prepend}p1:{prepend}p2", [
                mkEnv (M.fromList [("v4", [QualifiedPath [Prepend] "p1",QualifiedPath [Prepend] "p2"] )])]),
               ("v5={append}p1 v5={append}p2", [
                mkEnv (M.fromList [("v5", [QualifiedPath [Append] "p1",QualifiedPath [Append] "p2"] )])]),
               ("v6={append}p1:{append}p2", [
                mkEnv (M.fromList [("v6", [QualifiedPath [Append] "p1",QualifiedPath [Append] "p2"] )])])
              ]

-}
