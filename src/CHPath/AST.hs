module CHPath.AST (
PathElement(..), Statement(..),
unwrapList
)
where
import Data.List (union,nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import System.Directory


data PathElement =
  Literal String |
  File String |
  PathList [PathElement] |
  EnvVar String |
  Subtract PathElement PathElement |
  Optional PathElement
  deriving (Show,Eq,Ord)

data Statement =
  Assignment String PathElement (Maybe PathElement) |
  Directory FilePath |
  DirectoryDef FilePath [Statement]
  deriving (Show,Eq)

unwrapList :: PathElement -> [PathElement]
unwrapList (PathList l) = l
unwrapList p = [p]
