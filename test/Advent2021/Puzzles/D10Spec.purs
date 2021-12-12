module Advent2021.Puzzles.D10Spec (spec) where

import Prelude
import Advent2021.Puzzles.D10 as D10
import Advent2021.Spec.Assertions (shouldSucceed)
import Data.BigInt as BigInt
import Test.Spec (Spec, describe, it)

input :: String
input =
  """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
"""

spec :: Spec Unit
spec =
  describe "Day 10" do
    it "computes syntax error score" do
      D10.part1 input `shouldSucceed` 26397
    it "computes autocomplete score" do
      D10.part2 input `shouldSucceed` BigInt.fromInt 288957
