module Advent2021.Puzzles.D25Spec
  ( spec
  ) where

import Prelude
import Advent2021.Puzzles.D25 as D25
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input :: String
input =
  """v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
"""

spec :: Spec Unit
spec =
  describe "Day 25" do
    it "finds when sea cucumbers stop moving" do
      D25.part1 input `shouldSucceed` 58
