module Advent2021.Puzzles.D1
  ( part1
  , part2
  ) where

import Prelude
import Data.Array (tail, zip)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, head, toArray)
import Data.Array.NonEmpty as NE
import Data.Either (Either, note)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))

part1 :: NonEmptyArray Int -> Int
part1 input = increases
  where
  countIncreases :: { last :: Int, increases :: Int } -> Int -> { last :: Int, increases :: Int }
  countIncreases { last, increases } curr =
    { last: curr
    , increases: increases + if curr > last then 1 else 0
    }

  { increases } = foldl countIncreases { last: head input, increases: 0 } input

part2 :: NonEmptyArray Int -> Either String Int
part2 input = do
  let
    input2 = NE.tail input
  input3 <- note "Invalid input: report must have at least 3 measurements" $ tail input2
  windowTuples <- note "Impossible: no window measurements" $ fromArray $ zip input3 $ zip input2 $ toArray input
  pure $ part1 $ map sumTuple windowTuples
  where
  sumTuple (Tuple a (Tuple b c)) = a + b + c
