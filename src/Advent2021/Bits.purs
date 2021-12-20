module Advent2021.Bits
  ( Bit(..)
  , BitString
  , _0
  , _1
  , fromBinaryDigit
  , fromBinaryString
  , fromHexDigit
  , fromHexString
  , fromInt
  , padZerosTo
  , toInt
  ) where

import Prelude
import Advent2021.Debug (undefined)
import Advent2021.Parsers (digit)
import Control.Alternative ((<|>))
import Data.Foldable (foldr)
import Data.Int (pow)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable1 (replicate1)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char)
import Text.Parsing.StringParser.Combinators (many1)

newtype Bit
  = Bit Boolean

derive newtype instance eqBit :: Eq Bit

_1 :: Bit
_1 = Bit true

_0 :: Bit
_0 = Bit false

fromBinaryDigit :: Parser Bit
fromBinaryDigit = (char '1' $> Bit true) <|> (char '0' $> Bit false)

type BitString
  = NonEmptyList Bit

fromBinaryString :: Parser BitString
fromBinaryString = many1 fromBinaryDigit

fromHexDigit :: Parser BitString
fromHexDigit =
  padZerosTo 4
    <$> fromInt
    <$> ( digit
          <|> (char 'A' $> 10)
          <|> (char 'B' $> 11)
          <|> (char 'C' $> 12)
          <|> (char 'D' $> 13)
          <|> (char 'E' $> 14)
          <|> (char 'F' $> 15)
      )

fromHexString :: Parser BitString
fromHexString = NEList.concat <$> many1 fromHexDigit

toInt :: BitString -> Int
toInt bits =
  fst
    $ foldr
        (\(Bit b) (Tuple acc i) -> (Tuple (acc + if b then 2 `pow` i else 0) (i + 1)))
        (Tuple 0 0)
        bits

fromInt :: Int -> BitString
fromInt n = undefined
  where
  fromIntR :: Int -> List Bit
  fromIntR x = undefined

padZerosTo :: Int -> BitString -> BitString
padZerosTo width bs =
  if l < width then
    replicate1 (width - l) _0 <> bs
  else
    bs
  where
  l = NEList.length bs
