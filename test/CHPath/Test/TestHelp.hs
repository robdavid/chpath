{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,UndecidableInstances,TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module CHPath.Test.TestHelp (
                        quickTest,(=$=),
                        arbitraryFname, arbitraryIdentifier,
                        ArbIdentifier(..), ArbFilePath(..), Wrapped(..),
                        module Test.HUnit,
                        module Test.QuickCheck
                       )
where
import Test.HUnit hiding (Node)
import Test.QuickCheck hiding (Testable)
import Test.QuickCheck.Test
import Data.List(delete)
import Control.Applicative
import Control.Monad

quickTest t = do
  r <- quickCheckWithResult stdArgs t 
  case r of
    Success _ _ _ -> return ()
    GaveUp _ l _ -> assertFailure $ joinStr $ map fst l
    Failure _ _ _ _ _ l _ -> assertFailure $ "Failed for:\n" ++ (joinStr $ map fst l)
    NoExpectedFailure _ l _ -> assertFailure $ "Failed for:\n" ++ (joinStr $ map fst l)

joinStr :: [String] -> String
joinStr [] = ""
joinStr [s] = s
joinStr (s:ss) = s ++ ('\n':joinStr ss)

arbitraryFname = elements ["/home","/usr","/bin","bin","local"]

class Wrapped a b | a -> b where
    unwrap :: a -> b

instance Wrapped a b => Wrapped [a] [b] where
    unwrap = map unwrap

instance Wrapped a b => Wrapped (Maybe a) (Maybe b) where
    unwrap (Just a) = Just $ unwrap a
    unwrap Nothing = Nothing

instance Wrapped Int Int where
    unwrap = id

instance Wrapped String String where
    unwrap = id

instance (Wrapped a b, Wrapped c d) => Wrapped (a,c) (b,d) where
    unwrap (x,y) = (unwrap x, unwrap y)

newtype ArbFilePath = ArbFilePath String deriving (Eq,Show,Ord)

instance Wrapped ArbFilePath String where
    unwrap (ArbFilePath s) = s

instance Arbitrary ArbFilePath where
    arbitrary = ArbFilePath <$> arbitraryFname
--    coarbitrary = undefined

newtype ArbIdentifier = ArbIdentifier String deriving (Eq,Show,Ord)

instance Wrapped ArbIdentifier String where
    unwrap (ArbIdentifier i) = i

instance Arbitrary ArbIdentifier where
    arbitrary = ArbIdentifier <$> arbitraryIdentifier
--    coarbitrary = undefined

firstIdentifierChar = ['A'..'Z'] ++ ['a'..'z']
identifierChar =  firstIdentifierChar ++ ['0'..'9']
arbitraryIdentifier = 
  (:) <$> elements firstIdentifierChar <*> remainingId
  where remainingId = do
          continue <- elements [True,True,True,False]
          if continue then (:) <$> (elements identifierChar) <*> arbitraryIdentifier 
                      else return []


--instance Applicative Gen where
--    pure = return
--    (<*>) = ap


-- Unordered equality
[] =$= [] = True
(a:as) =$= bs = if (elem a bs) then as =$= (delete a bs) else False
