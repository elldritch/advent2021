module Advent2021.Puzzles.D15Spec (spec) where

import Prelude
import Advent2021.Puzzles.D15 as D15
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input =
  """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
"""

spec :: Spec Unit
spec =
  describe "Day 15" do
    it "finds the lowest risk path" do
      D15.part1 input `shouldSucceed` 40
    it "finds paths through full maps" do
      D15.part2 input `shouldSucceed` 315
