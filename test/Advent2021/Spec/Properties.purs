module Advent2021.Spec.Properties
  ( Positive(..)
  ) where

import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)

newtype Positive
  = Positive Int

instance arbitraryPositive :: Arbitrary Positive where
  arbitrary = Positive <$> arbitrary `suchThat` (_ > 0)
