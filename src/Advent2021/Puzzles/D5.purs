module Advent2021.Puzzles.D5
  ( part1
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.List (List, concat, foldl, range)
import Data.List as List
import Data.Map (empty, size)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence.Ordered as OrdSeq
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
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
  else
    Nothing

part1 :: String -> Either String Int
part1 input = do
  lines <- List.filter axisAligned <$> runParser inputP input
  ps <- map concat $ note "Impossible: line was not axis-aligned" $ sequence $ points <$> lines
  let
    counts = foldl (\m p -> Map.insertWith (+) p 1 m) empty $ OrdSeq.fromFoldable ps
  pure $ size $ Map.filter (_ > 1) counts
  where
  inputP :: Parser (List Line)
  inputP = sepEndBy lineP newline <* eof
