module Advent2021.Puzzles.D12
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Helpers (uniqueCounts)
import Advent2021.Parsers (newline, runParser)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (any, fold, length, notElem)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), concat, mapMaybe, reverse, (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (joinWith) as String
import Data.String (toLower, toUpper)
import Data.String.CodeUnits (fromCharArray) as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (anyLetter, char, eof)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy)

data Cave
  = Big String
  | Small String

derive instance eqCave :: Eq Cave

derive instance ordCave :: Ord Cave

derive instance genericCave :: Generic Cave _

instance showCave :: Show Cave where
  show = genericShow

caveP :: Parser Cave
caveP = do
  letters <- many1 anyLetter
  let
    name = String.fromCharArray $ Array.fromFoldable letters
  if toUpper name == name then
    pure $ Big name
  else if toLower name == name then
    pure $ Small name
  else
    fail $ "Could not determine cave size with name: " <> show name

type Connections
  = Map Cave (Set Cave)

showGraph :: forall a. Show a => Map a (Set a) -> String
showGraph m =
  String.joinWith "\n"
    $ (\(Tuple vertex edges) -> show vertex <> ": " <> show edges)
    <$> Map.toUnfoldable m

connectionsP :: Parser Connections
connectionsP = unwrap <<< fold <<< map SemigroupMap <$> sepEndBy connectionP newline
  where
  connectionP :: Parser Connections
  connectionP = do
    a <- caveP
    _ <- char '-'
    b <- caveP
    pure $ Map.fromFoldable [ Tuple a $ Set.singleton b, Tuple b $ Set.singleton a ]

type Path
  = List Cave

showPath :: Path -> String
showPath path =
  String.joinWith ","
    $ map
        ( case _ of
            Big b -> b
            Small s -> s
        )
    $ Array.fromFoldable path

showPaths :: List Path -> String
showPaths paths =
  show (List.length paths)
    <> ": \n"
    <> String.joinWith "\n" (map showPath $ Array.fromFoldable paths)

type CanVisitFunc
  = { visited :: List Cave, reachable :: Set Cave } -> Set Cave

run :: CanVisitFunc -> String -> Either String Int
run getVisitable input = do
  connections <- runParser (connectionsP <* eof) input
  paths <- findPaths getVisitable connections
  pure $ length paths

findPaths :: CanVisitFunc -> Connections -> Either String (List Path)
findPaths getVisitable connections = findPathsR Nil (Small "start")
  where
  findPathsR :: List Cave -> Cave -> Either String (List Path)
  findPathsR visited curr =
    if curr == Small "end" then
      pure $ List.singleton $ reverse $ curr : visited
    else do
      reachable <- note "Impossible: cave is not in map" $ Map.lookup curr connections
      let
        visited' = curr : visited

        visitable = Set.toUnfoldable $ getVisitable { visited: visited', reachable }
      concat <$> sequence (findPathsR visited' <$> visitable)

part1 :: String -> Either String Int
part1 =
  run \{ visited, reachable } ->
    Set.filter
      ( case _ of
          Big _ -> true
          Small c -> notElem (Small c) visited
      )
      reachable

part2 :: String -> Either String Int
part2 =
  run \{ visited, reachable } ->
    Set.filter
      ( case _ of
          Big _ -> true
          Small "start" -> false
          Small c ->
            if anySmallCaveVisitedTwice visited then
              notElem (Small c) visited
            else
              true
      )
      reachable
  where
  anySmallCaveVisitedTwice :: List Cave -> Boolean
  anySmallCaveVisitedTwice =
    any (_ >= 2)
      <<< Map.values
      <<< uniqueCounts
      <<< mapMaybe
          ( case _ of
              Big _ -> Nothing
              Small s -> Just s
          )
