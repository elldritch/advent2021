module Advent2021.Puzzles.D5
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Helpers (uniqueCounts)
import Advent2021.Parsers (integer, newline, runParser)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.List (List, concat, range, zip)
import Data.List as List
import Data.Map (size)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof, string)
import Text.Parsing.StringParser.Combinators (sepEndBy)

data Point
  = Point Int Int

derive instance genericPoint :: Generic Point _

derive instance eqPoint :: Eq Point

derive instance ordPoint :: Ord Point

instance showPoint :: Show Point where
  show = genericShow

pointP :: Parser Point
pointP = Point <$> integer <* char ',' <*> integer

data Line
  = Line Point Point

derive instance genericLine :: Generic Line _

instance showLine :: Show Line where
  show = genericShow

lineP :: Parser Line
lineP = Line <$> pointP <* string " -> " <*> pointP

axisAligned :: Line -> Boolean
axisAligned (Line (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

points :: Line -> Maybe (List Point)
points (Line (Point x1 y1) (Point x2 y2)) =
  if x1 == x2 then
    Just $ Point x1 <$> range y1 y2
  else if y1 == y2 then
    Just $ (\x -> Point x y1) <$> range x1 x2
  else if abs (x1 - x2) == abs (y1 - y2) then
    Just $ uncurry Point <$> zip (range x1 x2) (range y1 y2)
  else
    Nothing

countOverlapPoints :: List Line -> Either String Int
countOverlapPoints lines = do
  ps <- map concat $ note "Invalid input: line is not vertical, horizontal, or diagonal" $ sequence $ points <$> lines
  pure $ size $ Map.filter (_ > 1) $ uniqueCounts ps

inputP :: Parser (List Line)
inputP = sepEndBy lineP newline <* eof

part1 :: String -> Either String Int
part1 input = countOverlapPoints =<< List.filter axisAligned <$> runParser inputP input

part2 :: String -> Either String Int
part2 input = runParser inputP input >>= countOverlapPoints
