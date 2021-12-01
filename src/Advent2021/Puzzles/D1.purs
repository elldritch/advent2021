module Advent2021.Puzzles.D1
  ( part1
  , part2
  ) where

import Prelude
import Data.Array (zip)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, head, toArray)
import Data.Array.Partial (tail)
import Data.Foldable (foldl)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

part1 :: NonEmptyArray Int -> Int
part1 input = increases
  where
  countIncreases :: { last :: Int, increases :: Int } -> Int -> { last :: Int, increases :: Int }
  countIncreases { last, increases } curr =
    { last: curr
    , increases: increases + if curr > last then 1 else 0
    }

  { increases } = foldl countIncreases { last: head input, increases: 0 } input

part2 :: NonEmptyArray Int -> Int
part2 input = part1 windows
  where
  input2 = unsafePartial tail $ toArray input

  input3 = unsafePartial tail input2

  windowTuples = unsafePartial $ fromJust $ fromArray $ zip input3 $ zip input2 $ toArray input

  sumTuple (Tuple a (Tuple b c)) = a + b + c

  windows = map sumTuple windowTuples
