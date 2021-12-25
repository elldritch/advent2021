module Advent2021.Puzzles.D24Spec
  ( spec
  ) where

import Prelude
import Advent2021.Bits (_0)
import Advent2021.Bits as Bits
import Advent2021.Parsers (runParser)
import Advent2021.Puzzles.D24 as D24
import Advent2021.Spec.Properties (Positive(..))
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck, quickCheck')

input1 :: String
input1 =
  """inp x
mul x -1
"""

input2 :: String
input2 =
  """inp z
inp x
mul z 3
eql z x
"""

input3 :: String
input3 =
  """inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2
"""

testProgram :: String -> List Int -> Either String D24.Registers
testProgram instructions inputs = do
  program <- runParser D24.inputP instructions
  D24.run program inputs

spec :: Spec Unit
spec =
  describe "Day 24" do
    it "runs an ALU" do
      quickCheck \x ->
        testProgram input1 (List.singleton x) === Right { w: 0, x: -x, y: 0, z: 0 }
      quickCheck \(Tuple x z) ->
        testProgram input2 (z : x : Nil) === Right { w: 0, x, y: 0, z: if z == x * 3 then 1 else 0 }
      quickCheck' 5 \(Positive w) ->
        let
          lsbBits = NEList.reverse $ Bits.fromInt $ w

          z = fromEnum $ NEList.head lsbBits

          y = fromEnum $ fromMaybe _0 $ NEList.index lsbBits 1

          x = fromEnum $ fromMaybe _0 $ NEList.index lsbBits 2

          w' = fromEnum $ fromMaybe _0 $ NEList.index lsbBits 3
        in
          testProgram input3 (List.singleton w) === Right { w: w', x, y, z }
