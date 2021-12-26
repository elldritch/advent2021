module Advent2021.Puzzles.D19
  ( part1
  , part2
  ) where

import Prelude
import Data.Either (Either)

{-

Pick the farthest beacons from each scanner, and look at the universe with a
beacon at the origin. Try to align beacons with only translations after doing
their transformations.

Check "missing" overlaps to see if beacons should be within scanner range or
not. Missing beacons must be outside of the other scanner's range, and there
must not be extra beacons within the other scanner's range.

-}

part1 :: String -> Either String Int
part1 input = pure 0

part2 :: String -> Either String Int
part2 input = pure 0
