module Advent2021.BitsSpec
  ( spec
  ) where

import Prelude
import Advent2021.Bits (_0, _1, fromInt, toInt)
import Advent2021.Spec.Assertions (fromJust)
import Control.Monad.Error.Class (class MonadThrow)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
      tc 4 [ _1, _0, _0]
      tc 5 [ _1, _0, _1]
      tc 13 [ _1, _1, _0, _1]
