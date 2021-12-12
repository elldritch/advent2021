module Advent2021.Puzzles.D12
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (newline, runParser)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (fold, notElem, length)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), concat, reverse, (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
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

findPaths :: Connections -> Either String (List Path)
findPaths connections = findPathsR Nil (Small "start")
  where
  findPathsR :: List Cave -> Cave -> Either String (List Path)
  findPathsR visited curr =
    if curr == Small "end" then
      pure $ List.singleton $ reverse $ curr : visited
    else do
      reachable <- note "Impossible: cave is not in map" $ Map.lookup curr connections
      let
        visitable =
          Set.toUnfoldable
            $ Set.filter
                ( \cave -> case cave of
                    Big _ -> true
                    Small c -> notElem (Small c) visited
                )
                reachable
      concat <$> sequence (findPathsR (curr : visited) <$> visitable)

part1 :: String -> Either String Int
part1 input = do
  connections <- runParser (connectionsP <* eof) input
  paths <- findPaths connections
  pure $ length paths

part2 :: String -> Either String Int
part2 input = pure 0
