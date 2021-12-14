module Advent2021.Puzzles.D6
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Helpers (uniqueCounts, iterateN)
import Advent2021.Parsers (integer, newline, runParser)
import Data.BigInt (BigInt, fromInt, toString)
import Data.Either (Either)
import Data.Foldable (sum)
import Data.List (List, range)
import Data.Map (Map, values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepBy)

type Timer
  = Int

type Count
  = BigInt

type Fishes
  = Map Timer Count

fzero :: Fishes
fzero = Map.fromFoldable $ map (\k -> Tuple k $ fromInt 0) $ range 0 8

step :: Fishes -> Fishes
step fish = Map.fromFoldable $ map (\k -> Tuple k $ nextCount k) $ range 0 8
  where
  nextCount :: Timer -> Count
  nextCount 8 = case Map.lookup 0 fish of
    Just n -> n
    Nothing -> fromInt 0

  nextCount 6 =
    sum
      $ ( \k -> case Map.lookup k fish of
            Just n -> n
            Nothing -> fromInt 0
        )
      <$> [ 0, 7 ]

  nextCount t = case Map.lookup (t + 1) fish of
    Just n -> n
    Nothing -> fromInt 0

simulate :: Int -> String -> Either String String
simulate days input = do
  fishes <- (_ `Map.union` fzero) <<< uniqueCounts <$> runParser inputP input
  let
    result = iterateN step fishes days
  pure $ toString $ sum $ values result
  where
  inputP :: Parser (List Timer)
  inputP = sepBy integer (char ',') <* newline <* eof

part1 :: String -> Either String String
part1 = simulate 80

part2 :: String -> Either String String
part2 = simulate 256
