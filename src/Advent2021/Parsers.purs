module Advent2021.Parsers
  ( integer
  , newline
  , runParser
  , token
  , word
  ) where

import Prelude
import Data.Array (fromFoldable)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.StringParser (Parser, fail, printParserError)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodePoints (anyDigit, char, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (many)

runParser :: forall a. Parser a -> String -> Either String a
runParser a b = lmap printParserError $ StringParser.runParser a b

token :: forall a. Parser a -> Parser a
token p = p <* whiteSpace

word :: String -> Parser String
word s = token $ string s

newline :: Parser Unit
newline = void $ char '\n'

integer :: Parser Int
integer = do
  digits <- fromFoldable <$> many anyDigit
  let
    int = fromCharArray digits
  case fromString int of
    Just i -> pure i
    Nothing -> fail $ "expected integer, got " <> show int
