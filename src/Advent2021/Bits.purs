module Advent2021.Bits
  ( Bit(..)
  , BitString
  , _0
  , _1
  , bit0P
  , bit1P
  , fromBinaryDigit
  , fromBinaryString
  , fromHexDigit
  , fromHexString
  , fromInt
  , padZerosTo
  , showBinaryString
  , toBigInt
  , toInt
  ) where

import Prelude
import Advent2021.Parsers (digit)
import Control.Alternative ((<|>))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality)
import Data.Foldable (fold, foldr)
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Maybe (Maybe(..))
import Data.Unfoldable1 (replicate1)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char)
import Text.Parsing.StringParser.Combinators (many1)

newtype Bit
  = Bit Boolean

derive newtype instance eqBit :: Eq Bit

instance showBit :: Show Bit where
  show (Bit true) = "1"
  show (Bit false) = "0"

_1 :: Bit
_1 = Bit true

bit1P :: Parser Bit
bit1P = char '1' $> Bit true

_0 :: Bit
_0 = Bit false

bit0P :: Parser Bit
bit0P = char '0' $> Bit false

derive newtype instance ordBit :: Ord Bit

derive newtype instance boundedBit :: Bounded Bit

derive newtype instance enumBit :: Enum Bit

derive newtype instance boundedEnumBit :: BoundedEnum Bit

fromBinaryDigit :: Parser Bit
fromBinaryDigit = bit1P <|> bit0P

type BitString
  = NonEmptyList Bit

fromBinaryString :: Parser BitString
fromBinaryString = many1 fromBinaryDigit

showBinaryString :: BitString -> String
showBinaryString = fold <<< map show

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
toInt = toInt' Int.pow

toBigInt :: BitString -> BigInt
toBigInt = toInt' BigInt.pow

two :: forall a. Semiring a => a
two = one + one

toInt' :: forall a. Semiring a => (a -> a -> a) -> BitString -> a
toInt' pow bits =
  _.number
    $ foldr
        (\(Bit b) { number, index } -> { number: number + if b then two `pow` index else zero, index: index + one })
        ({ number: zero, index: zero })
        bits

fromInt :: Int -> BitString
fromInt n = fromIntR (n / 2) $ NEList.singleton $ if n `mod` 2 == 1 then _1 else _0
  where
  fromIntR :: Int -> BitString -> BitString
  fromIntR 0 bs = bs

  fromIntR x bs = fromIntR (x / 2) $ NEList.cons (if x `mod` 2 == 1 then _1 else _0) bs

padZerosTo :: Int -> BitString -> BitString
padZerosTo width bs =
  if l < width then
    replicate1 (width - l) _0 <> bs
  else
    bs
  where
  l = NEList.length bs
