module Advent2021.Spec.Parsers
  ( shouldParseTo
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.StringParser (ParseError, printParserError)

shouldParseTo :: forall a m. Eq a => Show a => MonadThrow Error m => (Either ParseError a) -> a -> m Unit
shouldParseTo parseResult expected = case parseResult of
  Right r -> r `shouldEqual` expected
  Left err -> fail $ printParserError err
