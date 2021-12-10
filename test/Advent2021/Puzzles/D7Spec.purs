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
    it "calculates minimum fuel needs" do
      D7.part1 input `shouldSucceed` 37
