module Advent2021.Puzzles.D7
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Data.Either (Either)
import Data.Foldable (sum)
import Data.List.NonEmpty (NonEmptyList)
import Data.Ord (abs)
import Data.Semigroup.Foldable (maximum, minimum, minimumBy)
import Data.Unfoldable1 (range)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepBy1)

type Position
  = Int

part1 :: String -> Either String Int
part1 input = do
  positions <- runParser inputP input
  let
    max = maximum positions

    min = minimum positions

    candidates = range min max :: NonEmptyList Position

    fuelNeeded candidate = sum $ map (\position -> abs $ position - candidate) positions
  pure $ fuelNeeded $ minimumBy (comparing fuelNeeded) candidates
  where
  inputP :: Parser (NonEmptyList Position)
  inputP = sepBy1 integer (char ',') <* newline <* eof

part2 :: String -> Either String Int
part2 = const $ pure 0
