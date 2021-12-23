module Advent2021.Puzzles.D14
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Helpers (iterateNM, uniqueCounts)
import Advent2021.Parsers (newline, runParser)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either, note)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), concat, concatMap, (:))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Semigroup.Foldable (maximum, minimum)
import Data.String (toUpper)
import Data.String.CodeUnits as StringCU
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import PointFree ((<..))
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (anyLetter, eof, string)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy)

type Element
  = Char

elementP :: Parser Element
elementP = do
  c <- anyLetter
  let
    s = StringCU.singleton c
  if toUpper s == s then
    pure c
  else
    fail $ "expected element, got " <> show c

type Pair
  = Tuple Element Element

type Polymer
  = List Pair

polymerP :: Parser Polymer
polymerP = do
  elements <- many1 elementP
  pure $ List.zip (NEList.toList elements) $ NEList.tail elements

type Rules
  = Map Pair (Tuple Pair Pair)

rulesP :: Parser Rules
rulesP = do
  left <- elementP
  right <- elementP
  _ <- string " -> "
  insert <- elementP
  pure
    $ Map.singleton (Tuple left right)
    $ Tuple (Tuple left insert) (Tuple insert right)

inputP :: Parser { template :: Polymer, rules :: Rules }
inputP = do
  template <- polymerP <* newline
  newline
  rules <- Map.unions <$> sepEndBy rulesP newline
  eof
  pure { template, rules }

type Pairs
  = Map Pair BigInt

initial :: Rules -> Polymer -> Pairs
initial rules = (_ `Map.union` zeros) <<< uniqueCounts
  where
  entries = Map.toUnfoldable rules

  pairs = concatMap (\(Tuple from (Tuple left right)) -> from : left : right : Nil) entries

  zeros = Map.fromFoldable $ map (_ `Tuple` BigInt.fromInt 0) pairs

step :: Rules -> Pairs -> Either String Pairs
step rules pairs = do
  producedEntries <-
    sequence
      $ map
          ( \(Tuple from (Tuple to1 to2)) -> do
              fromCount <- lookup' from pairs
              pure $ (Tuple to1 fromCount) : (Tuple to2 fromCount) : Nil
          )
          rulesEntries
  let
    produced =
      Map.filter (_ > BigInt.fromInt 0)
        $ Map.fromFoldableWith (+)
        $ concat
        $ producedEntries

    consumed =
      foldl
        (\counts (Tuple from _) -> Map.insert from (BigInt.fromInt 0) counts)
        pairs
        rulesEntries
  pure $ Map.unions [ produced, consumed, pairs ]
  where
  rulesEntries :: List (Tuple Pair (Tuple Pair Pair))
  rulesEntries = Map.toUnfoldable rules

  lookup' :: forall k v. Ord k => k -> Map k v -> Either String v
  lookup' = note "Impossible: unknown pair" <.. Map.lookup

countElements :: Pairs -> Map Element BigInt
countElements pairs =
  map (\n -> n / BigInt.fromInt 2 + n `mod` (BigInt.fromInt 2))
    $ foldlWithIndex
        ( \(Tuple e1 e2) counts count ->
            Map.insertWith (+) e1 count $ Map.insertWith (+) e2 count counts
        )
        Map.empty
        pairs

simulate :: Int -> String -> Either String BigInt
simulate n input = do
  { template, rules } <- runParser inputP input
  polymerized <- iterateNM (step rules) (initial rules template) n
  counts <-
    note "Invalid input: polymer has no pairs"
      $ NEList.fromFoldable
      $ Map.values
      $ countElements polymerized
  let
    maxCount = maximum counts

    minCount = minimum counts
  pure $ maxCount - minCount

part1 :: String -> Either String BigInt
part1 = simulate 10

part2 :: String -> Either String BigInt
part2 = simulate 40
