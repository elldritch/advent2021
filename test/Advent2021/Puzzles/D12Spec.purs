module Advent2021.Puzzles.D12Spec (spec) where

import Prelude
import Advent2021.Puzzles.D12 as D12
import Advent2021.Spec.Assertions (shouldSucceed)
import Test.Spec (Spec, describe, it)

input1 :: String
input1 =
  """start-A
start-b
A-c
A-b
b-d
A-end
b-end
"""

input2 :: String
input2 =
  """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
"""

input3 :: String
input3 =
  """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
"""

spec :: Spec Unit
spec =
  describe "Day 12" do
    it "counts paths through caves" do
      D12.part1 input1 `shouldSucceed` 10
      D12.part1 input2 `shouldSucceed` 19
      D12.part1 input3 `shouldSucceed` 226
    it "counts paths with revisits" do
      D12.part2 input1 `shouldSucceed` 36
      D12.part2 input2 `shouldSucceed` 103
      D12.part2 input3 `shouldSucceed` 3509
