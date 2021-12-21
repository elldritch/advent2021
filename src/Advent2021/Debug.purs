module Advent2021.Debug
  ( spy'
  , spyS'
  , undefined
  ) where

import Prelude
import Debug (class DebugWarning, spy, spyWith)
import Effect.Exception (throw)
import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

undefined :: Warn (Text "usage of undefined") => forall a. a
undefined = unsafeCoerce $ throw "undefined"

debug :: Boolean
debug = false

spy' :: forall a. DebugWarning => String -> a -> a
spy' msg = if debug then spy msg else identity

spyS' :: forall a. DebugWarning => Show a => String -> a -> a
spyS' msg = if debug then spyWith msg show else identity
