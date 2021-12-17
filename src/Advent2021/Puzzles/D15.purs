module Advent2021.Puzzles.D15
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, adjacent, gridP)
import Advent2021.Grid as Grid
import Advent2021.Parsers (runParser)
import Control.Monad.Rec.Class (Step(..), tailRecM, tailRecM2)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (foldl, sum)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty as NESet
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser.CodePoints (eof)

type Risk
  = Int

type Cavern
  = Grid Risk

type Distance
  = Int

type ShortestPath
  = { from :: Position
    , distance :: Distance
    , estimate :: Distance
    }

aStar ::
  (Position -> List (Tuple Position Distance)) ->
  (Position -> Distance) ->
  Position ->
  Position ->
  Either String (NonEmptyList Position)
aStar next heuristic start destination = do
  completeFroms <-
    tailRecM2
      aStarR
      (Set.singleton start)
      (Map.singleton start { from: start, distance: 0, estimate: heuristic start })
  reconstructPath completeFroms
  where
  reconstructPath :: Map Position ShortestPath -> Either String (NonEmptyList Position)
  reconstructPath completeFroms = tailRecM reconstructPathR (NEList.singleton destination)
    where
    reconstructPathR :: NonEmptyList Position -> Either String (Step (NonEmptyList Position) (NonEmptyList Position))
    reconstructPathR path =
      let
        curr = NEList.head path
      in
        if curr == start then
          pure $ Done $ NEList.reverse path
        else do
          { from } <- note "Impossible: reconstructing path with unknown vertex" $ Map.lookup curr completeFroms
          pure $ Loop $ NEList.cons from path

  -- This is a stack-safe variant of sortBy. See https://github.com/purescript/purescript-lists/issues/192.
  sortBy' :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList a
  sortBy' cmp =
    unsafePartial fromJust
      <<< NEList.fromFoldable
      <<< Array.sortBy cmp
      <<< Array.fromFoldable

  aStarR ::
    Set Position ->
    Map Position ShortestPath ->
    Either String (Step { a :: Set Position, b :: Map Position ShortestPath } (Map Position ShortestPath))
  aStarR queue best = do
    neQueue <- note "Destination not reachable" $ NESet.fromSet queue
    let
      pQueue =
        map fst
          $ sortBy' (\(Tuple _ a) (Tuple _ b) -> comparingDistance a b)
          $ map (\position -> Tuple position (_.estimate <$> Map.lookup position best))
          $ NESet.toUnfoldable1 neQueue

      current = NEList.head pQueue
    if current == destination then
      pure $ Done best
    else do
      currentDistance <-
        map _.distance
          $ note "Impossible: current vertex has never been seen before"
          $ Map.lookup current best
      let
        frontier = next current

        { best', toExplore } = foldl (update { current, currentDistance }) { best': best, toExplore: Nil } frontier

        queue' = Set.fromFoldable $ (NEList.tail pQueue) <> toExplore
      pure $ Loop { a: queue', b: best' }
    where
    update ::
      { current :: Position, currentDistance :: Distance } ->
      { best' :: Map Position ShortestPath, toExplore :: List Position } ->
      Tuple Position Distance ->
      { best' :: Map Position ShortestPath, toExplore :: List Position }
    update { current, currentDistance } acc@{ best', toExplore } (Tuple neighbor neighborDistance) = case Map.lookup neighbor best' of
      Just { distance } -> if distance' < distance then new else acc
      Nothing -> new
      where
      distance' = currentDistance + neighborDistance

      new =
        { best':
            Map.insert
              neighbor
              { from: current, distance: distance', estimate: distance' + heuristic neighbor }
              best'
        , toExplore: neighbor : toExplore
        }

  comparingDistance :: Maybe Distance -> Maybe Distance -> Ordering
  comparingDistance (Just _) Nothing = LT

  comparingDistance Nothing (Just _) = GT

  comparingDistance Nothing Nothing = EQ

  comparingDistance (Just a) (Just b) = compare a b

part1 :: String -> Either String Int
part1 input = do
  cavern <- runParser (gridP identity <* eof) input
  let
    start = { x: 0, y: 0 }

    destination = (\{ x, y } -> { x: x - 1, y: y - 1 }) $ Grid.dimensions cavern
  path <-
    aStar
      (adjacent cavern)
      (\{ x, y } -> abs (destination.x - x) + abs (destination.y - y))
      start
      destination
  startRisk <- note "Impossible: cavern is empty" $ Grid.lookup cavern start
  risks <- traverse (note "Impossible: path contains unknown vertex" <<< Grid.lookup cavern) path
  pure $ sum risks - startRisk

part2 :: String -> Either String Int
part2 input = pure 0
