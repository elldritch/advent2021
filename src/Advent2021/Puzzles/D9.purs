module Advent2021.Puzzles.D9
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (digit, newline, runParser)
import Data.Either (Either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), all, catMaybes, concat, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (sum)
import Data.Tuple (Tuple(..), snd)
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

adjacents :: HeightMap -> Position -> List Height
adjacents hm { x, y } =
  catMaybes do
    delta <- -1 : 1 : Nil
    let
      dx = Map.lookup { x: x + delta, y } hm

      dy = Map.lookup { x, y: y + delta } hm
    dx : dy : Nil

part1 :: String -> Either String Int
part1 input = do
  heightMap <- runParser heightMapP input
  let
    locations = Map.toUnfoldable heightMap :: List (Tuple Position Height)

    lowPoints = List.filter (\(Tuple position height) -> all (_ > height) $ adjacents heightMap position) locations
  pure $ sum $ ((_ + 1) <<< snd) <$> lowPoints

part2 :: String -> Either String Int
part2 = const $ pure 0
