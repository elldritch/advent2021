module Advent2021.Puzzles.D25
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, gridP)
import Advent2021.Grid as Grid
import Advent2021.Helpers (fixM')
import Advent2021.Parsers (runParser)
import Control.Alternative ((<|>))
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.List.NonEmpty as NEList
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)

data Location
  = EastCucumber
  | SouthCucumber
  | Empty

derive instance eqLocation :: Eq Location

instance showLocation :: Show Location where
  show EastCucumber = ">"
  show SouthCucumber = "v"
  show Empty = "."

locationP :: Parser Location
locationP =
  (char '>' $> EastCucumber)
    <|> (char 'v' $> SouthCucumber)
    <|> (char '.' $> Empty)

type Seafloor
  = Grid Location

inputP :: Parser Seafloor
inputP = gridP locationP <* eof

step :: Seafloor -> Either String Seafloor
step seafloor = do
  eastMoved <- movedFrom fromWest EastCucumber seafloor
  southMoved <- movedFrom fromNorth SouthCucumber eastMoved
  pure southMoved
  where
  dims = Grid.dimensions seafloor

  fromWest { x, y } = { x: if x == 0 then dims.x - 1 else x - 1, y }

  fromNorth { x, y } = { x, y: if y == 0 then dims.y - 1 else y - 1 }

  movedFrom :: (Position -> Position) -> Location -> Seafloor -> Either String Seafloor
  movedFrom from herd before =
    foldl
      ( \acc (Tuple { x, y } v) -> do
          seafloor' <- acc
          let
            prevPos = from { x, y }
          prevValue <-
            note "Impossible: lookup for east cucumbers out-of-bounds" $ Grid.lookup before prevPos
          if v == Empty && prevValue == herd then
            pure
              $ Grid.update (const Empty) prevPos
              $ Grid.update (const herd) { x, y } seafloor'
          else
            pure seafloor'
      )
      (Right before)
      $ NEList.filter (\(Tuple _ v) -> v == Empty)
      $ Grid.toUnfoldable1 before

part1 :: String -> Either String Int
part1 input = do
  floor <- runParser inputP input
  { iterations } <- fixM' step floor
  pure $ iterations + 1

part2 :: String -> Either String Int
part2 input = pure 0
