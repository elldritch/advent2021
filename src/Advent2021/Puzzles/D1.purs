module Advent2021.Puzzles.D1
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Data.Either (Either, note)
import Data.Foldable (class Foldable, foldl)
import Data.List (List, tail, zip)
import Data.List.NonEmpty (head)
import Data.List.NonEmpty as NEList
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof)
import Text.Parsing.StringParser.Combinators (sepEndBy)

run :: (List Int -> Either String Int) -> String -> Either String Int
run count input = do
  xs <- runParser inputP input
  count xs
  where
  inputP :: Parser (List Int)
  inputP = sepEndBy integer newline <* eof

countIncreases :: forall f a. Foldable f => Ord a => f a -> Int
countIncreases xs = case NEList.fromFoldable xs of
  Just xs' -> _.increases $ foldl f { last: head xs', increases: 0 } xs'
  Nothing -> 0
  where
  f :: { last :: a, increases :: Int } -> a -> { last :: a, increases :: Int }
  f { last, increases } curr =
    { last: curr
    , increases: increases + if curr > last then 1 else 0
    }

part1 :: String -> Either String Int
part1 = run $ \input -> pure $ countIncreases input

part2 :: String -> Either String Int
part2 =
  run
    $ \input -> do
        input2 <- note "Invalid input: report must have at least 3 measurements" $ tail input
        input3 <- note "Invalid input: report must have at least 3 measurements" $ tail input2
        let
          windowTuples = zip input3 $ zip input2 $ input
        pure $ countIncreases $ map sumTuple windowTuples
  where
  sumTuple (Tuple a (Tuple b c)) = a + b + c
