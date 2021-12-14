module Advent2021.Puzzles.D14
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Helpers (iterateN, uniqueCounts)
import Advent2021.Parsers (newline, runParser)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Either (Either, note)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.Semigroup.Foldable (maximum, minimum)
import Data.String (toUpper)
import Data.String.CodeUnits as StringCU
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (anyLetter, string)
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

type Polymer
  = NonEmptyList Element

polymerP :: Parser Polymer
polymerP = many1 elementP

showPolymer :: Polymer -> String
showPolymer = StringCU.fromCharArray <<< Array.fromFoldable

type InsertionRules
  = Map (Tuple Element Element) Element

insertionRuleP :: Parser InsertionRules
insertionRuleP = do
  left <- elementP
  right <- elementP
  _ <- string " -> "
  insert <- elementP
  pure $ Map.singleton (Tuple left right) insert

inputP :: Parser { template :: Polymer, rules :: InsertionRules }
inputP = do
  template <- polymerP <* newline
  newline
  rules <- Map.unions <$> sepEndBy insertionRuleP newline
  pure { template, rules }

step :: InsertionRules -> Polymer -> Polymer
step rules polymer = wrap $ NEList.head polymer :| tail'
  where
  tail' :: List Element
  tail' =
    foldr
      ( \pair@(Tuple _ right) xs -> case Map.lookup pair rules of
          Just insert -> insert : right : xs
          Nothing -> right : xs
      )
      Nil
      $ pairs polymer

  pairs :: forall a. NonEmptyList a -> List (Tuple a a)
  pairs xs = List.zip (NEList.toList xs) $ NEList.tail xs

part1 :: String -> Either String BigInt
part1 input = do
  { template, rules } <- runParser inputP input
  let
    polymerized = iterateN (step rules) template 10
  counts <- note "Invalid input: polymer is empty" $ NEList.fromFoldable $ Map.values $ uniqueCounts polymerized
  let
    maxCount = maximum counts

    minCount = minimum counts
  pure $ maxCount - minCount

part2 :: String -> Either String BigInt
part2 input = do
  { template, rules } <- runParser inputP input
  let
    polymerized = iterateN (step rules) template 40
  counts <- note "Invalid input: polymer is empty" $ NEList.fromFoldable $ Map.values $ uniqueCounts polymerized
  let
    maxCount = maximum counts

    minCount = minimum counts
  pure $ maxCount - minCount
