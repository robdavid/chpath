{-# LANGUAGE RelaxedPolyRec,TypeSynonymInstances,FlexibleInstances #-}
module CHPath.Grammar where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Data.Char
import Control.Monad (ap, foldM)
import Control.Applicative (Applicative(..), pure, (<*>), (<*), (*>), (<$>))
import CHPath.AST

lang = emptyDef {
         P.commentLine = "#",
         P.reservedNames = [ "promote", "append", "prepend", "replace", "literal", "directory", "include", "dirdef" ]
       }
lexer = P.makeTokenParser lang

identifier = P.identifier lexer
symbol = P.symbol lexer
comma = P.comma lexer
colon = P.colon lexer
whiteSpace = P.whiteSpace lexer
stringLiteral = P.stringLiteral lexer
reserved = P.reserved lexer
braces = P.braces lexer
brackets = P.brackets lexer
parens = P.parens lexer

--instance Applicative (GenParser s a) where
--    pure = return
--   (<*>) = ap

qualifier :: Parser Qualifier
qualifier = (reserved "append" *> return Append) <|>
            (reserved "prepend" *> return Prepend) <|>
            (reserved "literal" *> return Literal) <|>
            (reserved "replace" *> return Replace) <|>
            (reserved "promote" *> (Promote <$> (qualifyPath <$> qualified <*> path)))


qualifiers :: Parser [Qualifier]
qualifiers = braces (sepBy qualifier comma)

qualified :: Parser [Qualifier]
qualified = qualifiers  <|> return []

assignment :: Parser Assignment
assignment = return Assignment <*> try (whiteSpace *> identifier) <*> (symbol "=" *> pathList)

pathList :: Parser PathList
pathList = PathList <$> sepBy pathElement colon

pathElement :: Parser PathElem
pathElement = do 
  whiteSpace 
  q <- qualified 
  (PathElemList q <$> (parens pathList)) <|> (PathElem q <$> path)

anyTrue :: [a-> Bool] -> a -> Bool
anyTrue l x = foldl (\b f -> b || f x) False l

noneTrue l x = not (anyTrue l x)

pathChar = satisfy (noneTrue [flip elem ":(){}", isSpace])


path :: Parser Path
path = Path <$> (stringLiteral <|> many pathChar)

directoryClause :: Parser Path
directoryClause = Path <$> (try (whiteSpace *> (reserved "directory" <|> reserved "include")) 
                                    *> whiteSpace *> (filePathOf <$> path))

directoryDef :: Parser DirectoryDef
directoryDef = DirectoryDef <$> ((try (whiteSpace *> reserved "dirdef")) *> whiteSpace *> path) <*>
               (whiteSpace *> braces (statementList))

statement :: Parser Statement
statement = ((QualifierStatement <$> qualifiers) <|> 
             (DirectoryStatement <$> directoryClause) <|> 
             (DirectoryDefStatement <$> directoryDef) <|> 
             (AssignmentStatement <$> assignment)) 
             <* whiteSpace

rcStatement :: Parser Statement
rcStatement = (DirectoryDefStatement <$> directoryDef)
              <* whiteSpace


defaultedEnvStatement :: String -> Parser Statement
defaultedEnvStatement s = ((AssignmentStatement <$> (try assignment)) <|> 
                           (AssignmentStatement <$> (Assignment s <$> pathList) <* whiteSpace)) <* eof

statementList :: Parser [Statement]
statementList = many statement <* whiteSpace

rcStatementList :: Parser [Statement]
rcStatementList = many rcStatement <* whiteSpace

rcDefinitions = rcStatementList <* eof
definitions =  statementList <* eof

-- Local Variables:
-- haskell-program-name: "ghci -package parsec-2.1.0.1"
-- End:

