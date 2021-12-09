module Advent2021.Puzzles.D6
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Data.Either (Either, note)
import Data.List (List, filter, length)
import Data.List.Lazy (drop, head, iterate)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepBy)

type Fish
  = Int

next :: List Fish -> List Fish
next yesterday = decremented `append` spawned
  where
  decremented = map (\n -> if n == 0 then 6 else n - 1) yesterday

  spawned = map (const 8) $ filter (_ == 0) yesterday

simulate :: Int -> String -> Either String Int
simulate days input = do
  fishes <- runParser inputP input
  result <- note "Impossible: infinite list had missing element" $ head $ drop days $ iterate next fishes
  pure $ length result
  where
  inputP :: Parser (List Fish)
  inputP = sepBy integer (char ',') <* newline <* eof

part1 :: String -> Either String Int
part1 = simulate 80

part2 :: String -> Either String Int
part2 = simulate 256
