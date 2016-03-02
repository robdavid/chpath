{-# LANGUAGE RelaxedPolyRec,TypeSynonymInstances,FlexibleInstances #-}
module CHPath.Grammar where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Data.Char
import Control.Monad (ap, foldM)
import Control.Applicative (Applicative(..), pure, (<*>), (<*), (*>), (<$>))
import CHPath.AST

{-
PATH = $PATH:.
PATH += .
PATH =+ [.]
PATH = [.]:$PATH:bin
PATH = ([.]):$PATH-(a:b:c):bincd
-}
lang = emptyDef {
         P.commentLine = "#",
         P.reservedNames = [ "dir", "directory", "include", "dirdef" ]
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
lexeme = P.lexeme lexer

anyTrue :: [a-> Bool] -> a -> Bool
anyTrue l x = foldl (\b f -> b || f x) False l

noneTrue l x = not (anyTrue l x)

pathChar = satisfy (noneTrue [flip elem ":(){}[]-\";,=+^", isSpace])
envChar = satisfy $ \c -> isAlphaNum c || c == '_'

lineWhiteSpace :: Parser String
lineWhiteSpace = many (satisfy (\c -> isSpace c && c /= '\n'))

pathStr :: Parser String
pathStr = stringLiteral <|> many1 pathChar

path :: Parser PathElement
path = File <$> pathStr

literal :: Parser PathElement
literal =Literal <$> brackets (stringLiteral <|> many pathChar)

envvar :: Parser PathElement
envvar = EnvVar <$> (symbol "@" *> many1 envChar)

optionalElement :: Parser PathElement
optionalElement = Optional <$> braces pathElement

--pathTerm = try subtraction <|> pathElement
pathTerm :: Parser PathElement
pathTerm = pathElement `chainl1` (try (whiteSpace *> symbol "-") *> whiteSpace *>  return Subtract)

pathElement = literal <|> envvar <|> optionalElement <|> parens pathList <|> path

pathList :: Parser PathElement
pathList = PathList <$> sepBy pathTerm (try (whiteSpace *> colon) <* whiteSpace)

assignment :: Parser Statement
assignment = do
  var <- identifier
  whiteSpace
  (Assignment var <$> try (symbol "+=" *> reassign var (\e l -> PathList (e:l))) <*> return Nothing) <|>
    (Assignment var <$> try (symbol "=+" *> reassign var (\e l -> PathList (l++[e]))) <*> return Nothing) <|>
    (Assignment var <$> try (symbol "-=" *> reassign var (\e l -> Subtract e (PathList l)))<*> return Nothing) <|>
    (Assignment var <$> (symbol "=" *> pathList) <*> optionMaybe (try (whiteSpace *> symbol "^") *> pathList))
  where reassign :: String -> (PathElement -> [PathElement] -> PathElement)-> Parser PathElement
        reassign var combinator = combinator (EnvVar var) . unwrapList <$> pathList

directoryDef :: Parser Statement
directoryDef = DirectoryDef <$> (reserved "dirdef" *> whiteSpace *> pathStr) <*>
               (whiteSpace *> braces (whiteSpace *> statementList <* whiteSpace))

directory :: Parser Statement
directory = Directory <$> ((reserved "directory" <|> reserved "dir" <|> reserved "include") *> whiteSpace *> pathStr)

statement :: Parser Statement
statement = directory <|>  directoryDef <|>  assignment

rcStatement :: Parser Statement
rcStatement = directoryDef

statementSep = lexeme $ optional (char ';') <* whiteSpace
cliStatementSep = lexeme $ optional (char ',') <* whiteSpace

statementList :: Parser [Statement]
statementList = many (try statement <* statementSep)

rcStatementList :: Parser [Statement]
rcStatementList = many (try rcStatement <* statementSep)

cliStatementList :: Parser [Statement]
cliStatementList = statement `sepBy` cliStatementSep

cliDefinitions = whiteSpace *> cliStatementList <* whiteSpace <* eof
rcDefinitions = whiteSpace *> rcStatementList <* eof
definitions =  whiteSpace *> statementList <* eof
