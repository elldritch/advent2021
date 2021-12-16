module Advent2021.Puzzles.D15
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, gridP)
import Advent2021.Grid as Grid
import Advent2021.Parsers (runParser)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef, modify, new, read, write)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Either (Either, note)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Semigroup.Foldable (minimum)
import Data.Traversable (traverse)
import Debug (spyWith)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser.CodePoints (eof)

type Risk
  = Int

type Cavern
  = Grid Risk

lowestRiskPath :: Grid Risk -> Risk
lowestRiskPath grid =
  ST.run do
    memoRef <- new Map.empty
    lowestRiskPathToR memoRef { x: 0, y: 0 }
  where
  { maxX, maxY } = (\{ x, y } -> { maxX: x - 1, maxY: y - 1 }) $ Grid.dimensions grid

  lookup' :: Position -> Risk
  lookup' p = unsafePartial $ fromJust $ Grid.lookup grid p

  lowestRiskPathToR :: forall r. STRef r (Map Position Risk) -> Position -> ST r Risk
  lowestRiskPathToR memoRef { x, y } = do
    if x == maxX && y == maxY then do
      let
        risk = 0
      _ <- modify (Map.insert { x, y } (spyWith ("put " <> show { x, y }) show $ risk)) memoRef
      pure risk
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
              memo <- spyWith "memo size" (show <<< Map.size) <$> read memoRef
              nextRisk <- case spyWith ("lookup " <> show { x, y }) show $ Map.lookup next memo of
                Just r -> pure r
                Nothing -> lowestRiskPathToR memoRef next
              risk <- pure $ lookup' next
              pure $ risk + nextRisk
          )
          nexts
      let
        risk = minimum risks
      _ <- modify (Map.insert { x, y } (spyWith ("put " <> show { x, y }) show $ risk)) memoRef
      pure risk

part1 :: String -> Either String Int
part1 input = do
  cavern <- runParser (gridP identity <* eof) input
  pure $ lowestRiskPath cavern

part2 :: String -> Either String Int
part2 input = pure 0
