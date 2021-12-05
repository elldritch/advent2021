module Advent2021.Debug
  ( undefined
  ) where

import Prelude
import Effect.Exception (throw)
import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

undefined :: Warn (Text "usage of undefined") => forall a. a
undefined = unsafeCoerce $ throw "undefined"
