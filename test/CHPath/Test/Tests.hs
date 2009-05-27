module CHPath.Test.Tests (runAll, tests, astTests, pathsTests) 
where

import CHPath.AST
import CHPath.Paths
import CHPath.Test.TestHelp
import CHPath.Test.AST
import CHPath.Test.Paths
import CHPath.Grammar

import Data.List
import Data.Monoid
import Data.Maybe
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S

import Text.ParserCombinators.Parsec

main = runAll

runAll = runTestTT tests

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

