module Advent2021.Debug
  ( undefined
  ) where

import Prelude
import Effect.Exception (throw)
import Unsafe.Coerce (unsafeCoerce)

undefined :: forall a. a
undefined = unsafeCoerce $ throw "undefined"
