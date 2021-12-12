module Advent2021.Puzzles.D9
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, Position, adjacent, gridP)
import Advent2021.Grid as Grid
import Advent2021.Parsers (runParser)
import Data.Either (Either)
import Data.Foldable (product)
import Data.List (List, all, sortBy, take)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ordering (invert)
import Data.Set (Set)
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
heightMapP = gridP identity <* eof

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
  pure
    $ product
    $ map Set.size
    $ take 3
    $ sortBy (\a b -> invert $ comparing Set.size a b)
    $ map (basin heightMap <<< fst)
    $ lowPoints heightMap
  where
  basin :: HeightMap -> Position -> Set Position
  basin heightMap start = basinR heightMap Set.empty $ List.singleton start

  basinR :: HeightMap -> Set Position -> List Position -> Set Position
  basinR heightMap seen queue = case List.uncons queue of
    Just { head, tail } ->
      let
        next = adjacent heightMap head

        notWalls = List.filter ((_ /= 9) <<< snd) next

        unseen = List.filter (not <<< flip Set.member seen <<< fst) notWalls

        seen' = seen <> Set.fromFoldable (map fst unseen)

        queue' = map fst unseen <> tail
      in
        basinR heightMap seen' queue'
    Nothing -> seen
