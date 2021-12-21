module Advent2021.Puzzles.D17Spec
  ( spec
  ) where

import Prelude
import Advent2021.Puzzles.D17 as D17
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input = "target area: x=20..30, y=-10..-5"

spec :: Spec Unit
spec =
  describe "Day 17" do
    it "calculates the highest position on a stylish trajectory" do
      D17.part1 input `shouldSucceed` 45
    it "calculates all possible initial velocities" do
      D17.part2 input `shouldSucceed` 112
