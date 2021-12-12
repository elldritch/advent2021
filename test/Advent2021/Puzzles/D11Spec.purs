module Advent2021.Puzzles.D11Spec (spec) where

import Prelude
import Advent2021.Puzzles.D11 as D11
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input =
  """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"""

spec :: Spec Unit
spec =
  describe "Day 11" do
    it "counts dumbo flashes" do
      D11.part1 input `shouldSucceed` 1656
    it "finds flash synchronizations" do
      D11.part2 input `shouldSucceed` 195
