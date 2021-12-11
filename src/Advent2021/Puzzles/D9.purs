module Advent2021.Puzzles.D9
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (digit, newline, runParser)
import Data.Either (Either)
import Data.Foldable (product)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), all, concat, mapMaybe, sortBy, take, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ordering (invert)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sum)
import Data.Tuple (Tuple(..), fst, snd)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof)
import Text.Parsing.StringParser.Combinators (many, sepEndBy)

type Position
  = { x :: Int, y :: Int }

type Height
  = Int

type HeightMap
  = Map Position Height

heightMapP :: Parser HeightMap
heightMapP = do
  digits <- sepEndBy (many digit) newline <* eof
  pure $ Map.fromFoldable $ addPositions digits
  where
  addPositions :: List (List Height) -> List (Tuple Position Height)
  addPositions heights =
    concat
      $ mapWithIndex
          ( \y row ->
              mapWithIndex
                ( \x height ->
                    Tuple { x, y } height
                )
                row
          )
          heights

adjacents :: HeightMap -> Position -> List (Tuple Position Height)
adjacents heightMap { x, y } = do
  delta <- -1 : 1 : Nil
  mapMaybe (\p -> Tuple p <$> lookup' p) $ { x: x + delta, y } : { x, y: y + delta } : Nil
  where
  lookup' p = Map.lookup p heightMap

lowPoints :: HeightMap -> List (Tuple Position Height)
lowPoints heightMap =
  List.filter
    (\(Tuple position height) -> all (_ > height) $ snd <$> adjacents heightMap position)
    $ Map.toUnfoldable heightMap

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
        next = adjacents heightMap head

        notWalls = List.filter ((_ /= 9) <<< snd) next

        unseen = List.filter (not <<< flip Set.member seen <<< fst) notWalls

        seen' = seen <> Set.fromFoldable (map fst unseen)

        queue' = map fst unseen <> tail
      in
        basinR heightMap seen' queue'
    Nothing -> seen
