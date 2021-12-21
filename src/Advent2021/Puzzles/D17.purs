module Advent2021.Puzzles.D17
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Data.Either (Either)
import Data.Foldable (sum)
import Data.List (range)
import Data.Ord (abs)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof, string)
import Text.Parsing.StringParser.Combinators (optional)

type Area
  = { x ::
        { high :: Int
        , low :: Int
        }
    , y ::
        { high :: Int
        , low :: Int
        }
    }

inputP :: Parser Area
inputP = do
  _ <- string "target area: "
  _ <- string "x="
  xLow <- integer
  _ <- string ".."
  xHigh <- integer
  _ <- string ", "
  _ <- string "y="
  yLow <- integer
  _ <- string ".."
  yHigh <- integer
  optional newline
  eof
  pure { x: { high: xHigh, low: xLow }, y: { high: yHigh, low: yLow } }

part1 :: String -> Either String Int
part1 input = do
  { y: { low: yBottom } } <- runParser inputP input
  let
    maxYVelocity = abs yBottom - 1
  pure $ sum $ range maxYVelocity 0

part2 :: String -> Either String Int
part2 input = pure 0
