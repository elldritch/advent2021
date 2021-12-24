module Advent2021.Puzzles.D22
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Control.Alternative ((<|>))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList, all)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Unfoldable (range)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof, string)
import Text.Parsing.StringParser.Combinators (sepEndBy1)

data Cube
  = On
  | Off

derive instance eqCube :: Eq Cube

data Action
  = TurnOn
  | TurnOff

actionP :: Parser Action
actionP = (string "on" $> TurnOn) <|> (string "off" $> TurnOff)

type Range
  = { min :: Int, max :: Int }

rangeP :: Parser Range
rangeP = do
  min <- integer
  _ <- string ".."
  max <- integer
  pure { min, max }

type RebootStep
  = { action :: Action
    , x :: Range
    , y :: Range
    , z :: Range
    }

rebootStepP :: Parser RebootStep
rebootStepP = do
  action <- actionP
  _ <- string " "
  _ <- string "x="
  x <- rangeP
  _ <- string ",y="
  y <- rangeP
  _ <- string ",z="
  z <- rangeP
  pure { action, x, y, z }

inputP :: Parser (NonEmptyList RebootStep)
inputP = sepEndBy1 rebootStepP newline <* eof

type Position
  = { x :: Int
    , y :: Int
    , z :: Int
    }

type Reactor
  = Map Position Cube

applyStep :: Reactor -> RebootStep -> Reactor
applyStep reactor { action, x, y, z } =
  foldl
    (\reactor' position -> Map.insert position state reactor')
    reactor
    cubes
  where
  state = case action of
    TurnOn -> On
    TurnOff -> Off

  cubes :: List Position
  cubes = do
    x' <- range x.min x.max
    y' <- range y.min y.max
    z' <- range z.min z.max
    pure { x: x', y: y', z: z' }

part1 :: String -> Either String Int
part1 input = do
  steps <- runParser inputP input
  let
    withinInitialization { min, max } = min >= -50 && max <= 50

    initializationProcedure =
      NEList.filter
        (\{ x, y, z } -> all withinInitialization [ x, y, z ])
        steps

    initializedReactor = foldl applyStep Map.empty initializationProcedure
  pure $ Map.size $ Map.filter (_ == On) initializedReactor

part2 :: String -> Either String BigInt
part2 input = pure $ BigInt.fromInt 0
