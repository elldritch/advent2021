module Advent2021.Puzzles.D7Spec (spec) where

import Prelude
import Advent2021.Puzzles.D7 as D7
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input = "16,1,2,0,4,2,7,1,2,14\n"

spec :: Spec Unit
spec =
  describe "Day 7" do
    it "calculates minimum fuel needs at constant burn rate" do
      D7.part1 input `shouldSucceed` 37
    it "calculates minimum fuel needs for non-linear burns" do
      D7.part2 input `shouldSucceed` 168
