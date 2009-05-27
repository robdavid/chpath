module CHPath.Test.AST where

import CHPath.AST
import CHPath.Test.TestHelp hiding (Path)
import Control.Applicative
import Control.Monad
import Data.List

instance Arbitrary Qualifier where
    arbitrary = do
      n <- choose (0,4) :: Gen Int
      case n of
        0 -> do -- Avoid infinite mutual recursion
          r <- choose (0,4)
          es <- sequence $ repeat $ elements [Append,Prepend,Replace,Literal]
          f <- arbitraryFname
          return $ Promote (QualifiedPath (take r es) f)
        1 -> return Append
        2 -> return Prepend
        3 -> return Replace
        _ -> return Literal
--    coarbitrary = undefined

instance Arbitrary QualifiedPath where
    arbitrary = QualifiedPath <$> (nub <$> arbitrary) <*> arbitraryFname
--    coarbitrary = undefined

instance Arbitrary Path where
    arbitrary = sequence [Path <$> arbitraryFname] >>= elements
--    coarbitrary = undefined

instance Arbitrary PathElem where
    arbitrary = do
      r <- choose(0,4)
      let pl = repeat $ (PathElem <$> arbitrary <*> arbitrary)
      sequence [PathElem <$> arbitrary <*> arbitrary, 
                          PathElemList <$> arbitrary <*> (PathList <$> sequence (take r pl))]
                >>= elements
--    coarbitrary = undefined

instance Arbitrary PathList where
    arbitrary = PathList <$> arbitrary 
--    coarbitrary = undefined


