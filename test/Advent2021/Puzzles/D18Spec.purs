module Advent2021.Puzzles.D18Spec
  ( spec
  ) where

import Prelude
import Advent2021.Puzzles.D18 as D18
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input1 :: String
input1 =
  """[1,2]
[[3,4],5]
"""

input2 :: String
input2 =
  """[[[[4,3],4],4],[7,[[8,4],9]]]
[1,1]
"""

input3 :: String
input3 =
  """[1,1]
[2,2]
[3,3]
[4,4]
"""

input4 :: String
input4 =
  """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
"""

input5 :: String
input5 =
  """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
"""

input6 :: String
input6 =
  """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
"""

input7 :: String
input7 =
  """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
"""

spec :: Spec Unit
spec =
  describe "Day 18" do
    it "adds snailfish numbers" do
      D18.part1 input1 `shouldSucceed` 143
      D18.part1 input2 `shouldSucceed` 1384
      D18.part1 input3 `shouldSucceed` 445
      D18.part1 input4 `shouldSucceed` 791
      D18.part1 input5 `shouldSucceed` 1137
      D18.part1 input6 `shouldSucceed` 3488
      D18.part1 input7 `shouldSucceed` 4140
    it "finds the largest pairwise sum" do
      D18.part2 input7 `shouldSucceed` 3993
