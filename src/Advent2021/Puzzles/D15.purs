module Advent2021.Puzzles.D15
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, adjacent, gridP)
import Advent2021.Grid as Grid
import Advent2021.Parsers (runParser)
import Data.Either (Either, note)
import Data.List (List, concatMap, (:))
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (minimum, sequence, sum)
import Data.Tuple (fst)
import Text.Parsing.StringParser.CodePoints (eof)

type Risk
  = Int

type Cavern
  = Grid Risk

type Path t
  = List t

pathsTo ::
  forall vertex.
  Ord vertex =>
  (vertex -> List vertex) ->
  vertex ->
  vertex ->
  List (Path vertex)
pathsTo next start destination = pathByR (Set.singleton start) start
  where
  pathByR :: Set vertex -> vertex -> List (Path vertex)
  pathByR seen curr =
    if curr == destination then
      List.singleton $ List.singleton curr
    else
      paths
    where
    frontier = next curr

    unseen = List.filter (_ `not Set.member` seen) frontier

    paths = map (curr : _) $ concatMap (pathByR $ Set.insert curr seen) unseen

part1 :: String -> Either String Int
part1 input = do
  cavern <- runParser (gridP identity <* eof) input
  let
    paths = leastRiskPath cavern
  risksOfPaths <- sequence $ map (riskOfPath cavern) paths
  minPathRisk <- note "Invalid input: no paths found" $ minimum risksOfPaths
  startRisk <- note "Invalid input: grid is empty" $ Grid.lookup cavern { x: 0, y: 0 }
  pure $ minPathRisk - startRisk
  where
  leastRiskPath :: Grid Risk -> List (Path Position)
  leastRiskPath grid =
    pathsTo
      -- Lemma: you always want to go right or left. Simple proof via dynamic programming solution.
      (\{ x, y } -> List.filter (\{ x: x', y: y' } -> x' > x || y' > y) $ map fst $ adjacent grid { x, y })
      { x: 0, y: 0 }
      ((\{ x, y } -> { x: x - 1, y: y - 1 }) $ Grid.dimensions grid)

  riskOfPath :: Grid Risk -> Path Position -> Either String Risk
  riskOfPath cavern path = do
    riskLevels <- note "Impossible: path is out-of-bounds" $ sequence $ map (Grid.lookup cavern) path
    pure $ sum riskLevels

part2 :: String -> Either String Int
part2 input = pure 0
