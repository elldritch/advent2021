module Advent2021.Parsers
  ( digit
  , integer
  , newline
  , runParser
  , space
  , token
  , word
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as StringCU
import Text.Parsing.StringParser (Parser, fail, printParserError)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodePoints (anyDigit, char, string)
import Text.Parsing.StringParser.Combinators (many)

runParser :: forall a. Parser a -> String -> Either String a
runParser a b = lmap printParserError $ StringParser.runParser a b

space :: Parser String
space = StringCU.fromCharArray <<< Array.fromFoldable <$> many (char ' ' <|> char '\t')

token :: forall a. Parser a -> Parser a
token p = p <* space

word :: String -> Parser String
word s = token $ string s

newline :: Parser Unit
newline = void $ char '\n'

digit :: Parser Int
digit = do
  n <- StringCU.singleton <$> anyDigit
  case Int.fromString n of
    Just i -> pure i
    Nothing -> fail $ "expected digit, got " <> show n

integer :: Parser Int
integer = do
  digits <- Array.fromFoldable <$> many anyDigit
  let
    int = StringCU.fromCharArray digits
  case Int.fromString int of
    Just i -> pure i
    Nothing -> fail $ "expected integer, got " <> show int
