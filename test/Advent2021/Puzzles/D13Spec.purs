module Advent2021.Puzzles.D13Spec (spec) where

import Prelude
import Advent2021.Puzzles.D13 as D13
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input =
  """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""

spec :: Spec Unit
spec =
  describe "Day 13" do
    it "counts dots after a fold" do
      D13.part1 input `shouldSucceed` 17
