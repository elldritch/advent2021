module Advent2021.Puzzles.D8
  ( part1
  ) where

import Prelude
import Advent2021.Parsers (newline, runParser, space, word)
import Data.Array as Array
import Data.Either (Either)
import Data.List (List, any, concat, filter, length)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Unfoldable (replicateA)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof, satisfy)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy)

type Signal
  = String

signalP :: Parser Signal
signalP = (fromCharArray <<< Array.fromFoldable) <$> many1 segmentP
  where
  segmentP :: Parser Char
  segmentP = satisfy \c -> c >= 'a' && c <= 'g'

type Display
  = { uniques :: List Signal, outputs :: List Signal }

displayP :: Parser Display
displayP = do
  uniques <- replicateA 10 (signalP <* space)
  _ <- word "|"
  outputs <- replicateA 4 (signalP <* space)
  pure { uniques, outputs }

part1 :: String -> Either String Int
part1 input = do
  displays <- runParser inputP input
  pure $ length $ concat $ map (\{ outputs } -> filter (\signal -> any (\n -> String.length signal == n) [ 2, 4, 3, 7 ]) outputs) displays
  where
  inputP :: Parser (List Display)
  inputP = sepEndBy displayP newline <* eof
