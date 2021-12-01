module Advent2021.Puzzles.D1
  ( part1
  ) where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Foldable (foldl)

part1 :: NonEmptyArray Int -> Int
part1 input =
  let
    { increasedCount } = foldl f { lastElement: head input, increasedCount: 0 } input
  in
    increasedCount
  where
  f :: { lastElement :: Int, increasedCount :: Int } -> Int -> { lastElement :: Int, increasedCount :: Int }
  f { lastElement, increasedCount } curr =
    { lastElement: curr
    , increasedCount: increasedCount + if curr > lastElement then 1 else 0
    }
