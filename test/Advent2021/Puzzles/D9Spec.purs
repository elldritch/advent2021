module Advent2021.Puzzles.D9Spec (spec) where

import Prelude
import Advent2021.Puzzles.D9 as D9
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input =
  """2199943210
3987894921
9856789892
8767896789
9899965678
"""

spec :: Spec Unit
spec =
  describe "Day 9" do
    it "finds low points" do
      D9.part1 input `shouldSucceed` 15
    it "finds basins" do
      D9.part2 input `shouldSucceed` 1134
