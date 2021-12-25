module Advent2021.BitsSpec
  ( spec
  ) where

import Prelude
import Advent2021.Bits (_0, _1, fromBinaryString, fromHexString, fromInt, showBinaryString, toInt)
import Advent2021.Parsers (runParser)
import Advent2021.Spec.Assertions (fromJust, fromRight)
import Advent2021.Spec.Properties (Positive(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Effect.Exception (Error)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

fromArray :: forall m a. MonadThrow Error m => Array a -> m (NonEmptyList a)
fromArray = fromJust <<< NEList.fromFoldable

spec :: Spec Unit
spec =
  describe "Bits" do
    it "decodes binary strings into integers" do
      input <- fromArray [ _1, _0 ]
      toInt input `shouldEqual` 2
    it "encodes integers into binary strings" do
      let
        tc n arr = do
          expected <- fromArray arr
          fromInt n `shouldEqual` expected
      tc 2 [ _1, _0 ]
      tc 4 [ _1, _0, _0 ]
      tc 5 [ _1, _0, _1 ]
      tc 13 [ _1, _1, _0, _1 ]
    it "renders binary strings" do
      showBinaryString (fromInt 2) `shouldEqual` "10"
      showBinaryString (fromInt 4) `shouldEqual` "100"
      showBinaryString (fromInt 5) `shouldEqual` "101"
      showBinaryString (fromInt 13) `shouldEqual` "1101"
    it "encoding and decoding bitstrings are inverses" do
      quickCheck \(Positive n) -> n === toInt (fromInt n)
    it "decodes hexadecimal strings" do
      actual <- fromRight $ runParser fromHexString "D2FE28"
      expected <- fromRight $ runParser fromBinaryString "110100101111111000101000"
      actual `shouldEqual` expected
