module Advent2021.Puzzles.D15
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, gridP)
import Advent2021.Grid as Grid
import Advent2021.Parsers (runParser)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Either (Either, note)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (minimum)
import Data.Traversable (traverse)
import Debug (spyWith)
import Text.Parsing.StringParser.CodePoints (eof)

type Risk
  = Int

type Cavern
  = Grid Risk

lowestRiskPath :: Grid Risk -> Either String Risk
lowestRiskPath grid = evalStateT (lowestRiskPathToR { x: 0, y: 0 }) Map.empty
  where
  { maxX, maxY } = (\{ x, y } -> { maxX: x - 1, maxY: y - 1 }) $ Grid.dimensions grid

  lookup' = note "Impossible: risk lookup out-of-bounds" <<< Grid.lookup grid

  lowestRiskPathToR :: Position -> StateT (Map Position Risk) (Either String) Risk
  lowestRiskPathToR { x, y } = do
    memo <- get
    case spyWith ("lookup " <> show { x, y }) show $ Map.lookup { x, y } memo of
      Just risk -> pure risk
      Nothing -> do
        result <-
          if x == maxX && y == maxY then do
            put $ Map.insert { x, y } (spyWith ("put " <> show { x, y }) show $ 0) memo
            pure 0
          else do
            let
              nexts =
                if y == maxY then
                  NEList.singleton { x: x + 1, y }
                else if x == maxX then
                  NEList.singleton { x, y: y + 1 }
                else
                  NEList.cons { x: x + 1, y } $ NEList.singleton { x, y: y + 1 }
            risks <-
              traverse
                ( \next -> do
                    risk <- lift $ lookup' next
                    nextRisk <- lowestRiskPathToR next
                    put $ Map.insert { x, y } (spyWith ("put " <> show { x, y }) show (risk + nextRisk)) memo
                    pure $ risk + nextRisk
                )
                nexts
            pure $ minimum risks
        put $ Map.insert { x, y } result memo
        pure result

part1 :: String -> Either String Int
part1 input = do
  cavern <- runParser (gridP identity <* eof) input
  lowestRiskPath cavern

part2 :: String -> Either String Int
part2 input = pure 0
