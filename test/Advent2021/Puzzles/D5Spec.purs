module Advent2021.Puzzles.D5Spec (spec) where

import Prelude
import Advent2021.Puzzles.D5 as D5
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"""

spec :: Spec Unit
spec =
  describe "Day 5" do
    it "counts points with multiple hydrothermal lines" do
      D5.part1 input `shouldSucceed` 5
    it "includes diagonal hydrothermal lines" do
      D5.part2 input `shouldSucceed` 12
