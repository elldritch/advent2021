module Advent2021.Puzzles.D9
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, adjacent, gridP)
import Advent2021.Grid as Grid
import Advent2021.Parsers (digit, runParser)
import Advent2021.Paths (reachable)
import Data.Either (Either)
import Data.Foldable (product)
import Data.List (List, all, sortBy, take)
import Data.List as List
import Data.Ordering (invert)
import Data.Set as Set
import Data.Traversable (sum)
import Data.Tuple (Tuple(..), fst, snd)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof)

type Height
  = Int

type HeightMap
  = Grid Height

heightMapP :: Parser HeightMap
heightMapP = gridP digit <* eof

lowPoints :: HeightMap -> List (Tuple Position Height)
lowPoints heightMap =
  List.filter
    (\(Tuple position height) -> all (_ > height) $ snd <$> adjacent heightMap position)
    $ Grid.toUnfoldable heightMap

part1 :: String -> Either String Int
part1 input = do
  heightMap <- runParser heightMapP input
  pure $ sum $ ((_ + 1) <<< snd) <$> lowPoints heightMap

part2 :: String -> Either String Int
part2 input = do
  heightMap <- runParser heightMapP input
  let
    neighbors :: Position -> List Position
    neighbors p = map fst $ List.filter ((_ /= 9) <<< snd) $ adjacent heightMap p
  pure
    $ product
    $ map Set.size
    $ take 3
    $ sortBy (\a b -> invert $ comparing Set.size a b)
    $ map (reachable neighbors <<< fst)
    $ lowPoints heightMap
