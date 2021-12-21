module Advent2021.Puzzles.D17
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Data.Either (Either)
import Data.Foldable (sum)
import Data.List (List, range)
import Data.List as List
import Data.List.Lazy (iterate)
import Data.List.Lazy as ListL
import Data.Maybe (isJust)
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
part2 input = do
  area <- runParser inputP input
  pure
    $ List.length
    $ List.nub
    $ List.concat
    $ launchVelocitiesFor
    <$> positionsIn area

type Position
  = { x :: Int, y :: Int }

type Velocity
  = { x :: Int, y :: Int }

type Tick
  = { position :: Position, velocity :: Velocity }

launchesTo :: Velocity -> Position -> Boolean
launchesTo launchVelocity target =
  isJust
    $ ListL.find (\{ position: { x, y } } -> x == target.x && y == target.y)
    $ ListL.takeWhile (\{ position: { x, y } } -> x <= target.x && y >= target.y)
    $ iterate step start
  where
  start :: Tick
  start = { position: { x: 0, y: 0 }, velocity: launchVelocity }

  step :: Tick -> Tick
  step { position, velocity } =
    { position:
        { x: position.x + velocity.x
        , y: position.y + velocity.y
        }
    , velocity:
        { x:
            if velocity.x == 0 then
              velocity.x
            else if velocity.x < 0 then
              velocity.x + 1
            else
              velocity.x - 1
        , y: velocity.y - 1
        }
    }

launchVelocitiesFor :: Position -> List Velocity
launchVelocitiesFor target = List.filter (_ `launchesTo` target) candidateVelocities
  where
  candidateVelocities :: List Velocity
  candidateVelocities = do
    x <- range 0 target.x
    y <- range target.y (abs target.y - 1)
    pure { x, y }

positionsIn :: Area -> List Position
positionsIn area = do
  x <- range area.x.low area.x.high
  y <- range area.y.low area.y.high
  pure { x, y }
