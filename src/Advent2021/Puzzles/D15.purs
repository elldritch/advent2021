module Advent2021.Puzzles.D15
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, adjacent, gridP)
import Advent2021.Grid as Grid
import Advent2021.Parsers (digit, runParser)
import Advent2021.Paths (aStar)
import Data.Either (Either, note)
import Data.Foldable (sum)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Ord (abs)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (range)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof)

type Risk
  = Int

type Cavern
  = Grid Risk

inputP :: Parser Cavern
inputP = gridP digit <* eof

lowestRiskPath :: Grid Risk -> Either String Int
lowestRiskPath cavern = do
  let
    start = { x: 0, y: 0 }

    destination = (\{ x, y } -> { x: x - 1, y: y - 1 }) $ Grid.dimensions cavern
  path <-
    aStar
      (map (\(Tuple position risk) -> { neighbor: position, distance: risk }) <<< adjacent cavern)
      (\{ x, y } -> abs (destination.x - x) + abs (destination.y - y))
      start
      destination
  startRisk <- note "Impossible: cavern is empty" $ Grid.lookup cavern start
  risks <- traverse (note "Impossible: path contains unknown vertex" <<< Grid.lookup cavern) path
  pure $ sum risks - startRisk

part1 :: String -> Either String Int
part1 input = do
  cavern <- runParser inputP input
  lowestRiskPath cavern

type GridEntries t
  = NonEmptyList (Tuple Position t)

part2 :: String -> Either String Int
part2 input = do
  cavern <- runParser inputP input
  let
    tiles :: GridEntries Risk
    tiles =
      NEList.concat do
        tileX <- range 0 4
        tileY <- range 0 4
        pure $ tile cavern { tileX, tileY }

    fullCavern = Grid.fromFoldable1 0 tiles
  lowestRiskPath fullCavern
  where
  tile :: Grid Risk -> { tileX :: Int, tileY :: Int } -> GridEntries Risk
  tile grid { tileX, tileY } =
    map
      ( \(Tuple { x, y } t) ->
          Tuple
            { x: x + (dims.x * tileX)
            , y: y + (dims.y * tileY)
            }
            ( let
                t' = t + tileX + tileY
              in
                if t' > 9 then ((t' - 1) `mod` 9) + 1 else t'
            )
      )
      entries
    where
    dims = Grid.dimensions grid

    entries = Grid.toUnfoldable1 grid
