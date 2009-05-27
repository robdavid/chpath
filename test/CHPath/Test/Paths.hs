{-# LANGUAGE MultiParamTypeClasses,TypeSynonymInstances, FlexibleInstances #-}
module CHPath.Test.Paths where

import CHPath.Paths
import Control.Applicative
import CHPath.Test.AST
import CHPath.Test.TestHelp
import Control.Monad
import qualified Data.Map as M

--- Test Support ---

arbitraryMaybeFname = join $ elements [return Nothing, Just <$> arbitraryFname]
instance Arbitrary RootedPath where
    arbitrary = (:@:) <$> arbitrary <*> arbitraryMaybeFname
--    coarbitrary = undefined

instance Wrapped RootedPath RootedPath where unwrap = id

instance Arbitrary ChangePath where
    arbitrary = PathEnvironment <$> M.fromList <$> unwrap <$> (arbitrary :: Gen [(ArbIdentifier,[RootedPath])])
--    coarbitrary = undefined
