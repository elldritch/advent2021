module Advent2021.Puzzles.D10Spec (spec) where

import Prelude
import Advent2021.Puzzles.D10 as D10
import Advent2021.Spec.Assertions (shouldSucceed)
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
    it "computes total syntax error score" do
      D10.part1 input `shouldSucceed` 26397
