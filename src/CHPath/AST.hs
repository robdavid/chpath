{-# LANGUAGE FlexibleInstances #-}
module CHPath.AST (
Qualifier(..), QualifiedPath(..), Qualified(..),
Assignment(..), PathList(..), Path(..), PathElem(..), 
Statement(..), PathLike(..), DirectoryDef(..),
qualifyPath,
qunion, qnub,
)
where
import Data.List (union,nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad

data Qualifier = Promote (QualifiedPath) |
                 Append |
                 Prepend |
                 Replace |
                 Literal -- Not canonizable
                 deriving (Show, Eq)


data Assignment = Assignment String PathList deriving (Eq,Show)
data PathList = PathList [PathElem] deriving (Eq,Show)
data Path = Path String deriving (Eq,Show)
data PathElem = PathElem  [Qualifier] Path | PathElemList [Qualifier] PathList deriving (Eq,Show)
data DirectoryDef = DirectoryDef Path [Statement] deriving (Eq,Show) 
data Statement = AssignmentStatement Assignment | 
                 DirectoryStatement Path | 
                 QualifierStatement [Qualifier] |
                 DirectoryDefStatement DirectoryDef
                 
                 deriving (Eq,Show)

data QualifiedPath = QualifiedPath [Qualifier] String deriving (Eq,Show)

qunion :: [Qualifier] -> [Qualifier] -> [Qualifier]
qunion a b = union (nub (if any apqual b then filter (not . apqual) a else a)) b
             where apqual q = q == Append || q == Prepend

qnub [] = []
qnub (q:qs) = qunion [q] (qnub qs)

qualifyPath :: [Qualifier] -> Path -> QualifiedPath
qualifyPath qs (Path s) = QualifiedPath qs s

class Qualified a where
    qualifiersOf :: a -> [Qualifier]
    qualify :: [Qualifier] -> a -> a
    isLiteral :: a -> Bool
    isLiteral a = Literal `elem` (qualifiersOf a)
    isAppend :: a -> Bool
    isAppend a = not $ isPrepend a
    isPrepend :: a -> Bool
    isPrepend a = Prepend `elem` (qualifiersOf a)
    isReplace :: a -> Bool
    isReplace a = Replace `elem` (qualifiersOf a)

    -- | Extracts the Promote qualifiers in a qualified value
    -- | and returns the list of qualified paths from the qualifiers
    promoted :: a -> [QualifiedPath]
    promoted a = mapMaybe select (qualifiersOf a)
                  where select (Promote p) = Just p
                        select _ = Nothing

instance Qualified [Qualifier] where
    qualifiersOf qs = qs
    qualify qs qs' = qunion qs qs'

instance Qualified QualifiedPath where
    qualifiersOf (QualifiedPath qs _) = qs
    qualify qs (QualifiedPath qs' p) = QualifiedPath (qualify qs qs') p

instance Qualified PathElem where
    qualifiersOf (PathElem qs _) = qs
    qualifiersOf (PathElemList qs _) = qs
    qualify qs (PathElem qs' p) = PathElem (qualify qs qs') p
    qualify qs (PathElemList qs' pl) = PathElemList (qualify qs qs') pl


class PathLike a where
    filePathOf :: a -> FilePath

instance PathLike Path where filePathOf (Path s) = s
instance PathLike QualifiedPath where filePathOf (QualifiedPath _ s) = s