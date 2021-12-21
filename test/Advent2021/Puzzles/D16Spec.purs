module Advent2021.Puzzles.D16Spec
  ( spec
  ) where

import Prelude
import Advent2021.Puzzles.D16 as D16
import Advent2021.Spec.Assertions (shouldSucceed)
import Data.BigInt as BigInt
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "Day 16" do
    it "sums version numbers of packet hierarchies" do
      D16.part1 "D2FE28" `shouldSucceed` 6
      D16.part1 "38006F45291200" `shouldSucceed` (1 + 6 + 2)
      D16.part1 "EE00D40C823060" `shouldSucceed` (7 + 2 + 4 + 1)
      D16.part1 "8A004A801A8002F478" `shouldSucceed` 16
      D16.part1 "620080001611562C8802118E34" `shouldSucceed` 12
      D16.part1 "C0015000016115A2E0802F182340" `shouldSucceed` 23
      D16.part1 "A0016C880162017C3686B18A3D4780" `shouldSucceed` 31
    it "evaluates packets" do
      D16.part2 "C200B40A82" `shouldSucceed` BigInt.fromInt 3
      D16.part2 "04005AC33890" `shouldSucceed` BigInt.fromInt 54
      D16.part2 "880086C3E88112" `shouldSucceed` BigInt.fromInt 7
      D16.part2 "CE00C43D881120" `shouldSucceed` BigInt.fromInt 9
      D16.part2 "D8005AC2A8F0" `shouldSucceed` BigInt.fromInt 1
      D16.part2 "F600BC2D8F" `shouldSucceed` BigInt.fromInt 0
      D16.part2 "9C005AC2F8F0" `shouldSucceed` BigInt.fromInt 0
      D16.part2 "9C0141080250320F1802104A08" `shouldSucceed` BigInt.fromInt 1
