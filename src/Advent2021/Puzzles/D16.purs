module Advent2021.Puzzles.D16
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Bits (Bit(..), BitString, bit0P, bit1P, fromBinaryDigit, fromHexString, showBinaryString)
import Advent2021.Bits as Bits
import Advent2021.Debug (spy', spyS')
import Advent2021.Parsers (runParser)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Foldable (foldr, product, sum)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEList
import Data.List.Types (NonEmptyList)
import Data.NonEmpty ((:|))
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Traversable (traverse)
import Data.Unfoldable1 (replicate1A)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (eof)
import Text.Parsing.StringParser.Combinators (many)

type TypeID
  = Int

data Packet
  = Packet { version :: Int, typeID :: TypeID, payload :: Payload }

bitsP :: Int -> Parser BitString
bitsP n = replicate1A n fromBinaryDigit

numP :: Int -> Parser Int
numP = map Bits.toInt <<< bitsP

packetP :: Parser { packet :: Packet, length :: Int }
packetP = do
  version <- numP 3
  typeID <- numP 3
  { payload, length: payloadLength } <- payloadP typeID
  pure $ { packet: Packet { version, typeID, payload }, length: payloadLength + 3 + 3 }

data Operation
  = Sum
  | Product
  | Minimum
  | Maximum
  | GreaterThan
  | LessThan
  | EqualTo

data Payload
  = Literal { value :: BigInt }
  | Operator { operation :: Operation, subPackets :: NonEmptyList Packet }

payloadP :: TypeID -> Parser { payload :: Payload, length :: Int }
payloadP 4 = do
  groups <- many $ groupP bit1P
  lastGroup <- groupP bit0P
  let
    bits = foldr (<>) lastGroup groups
  pure $ { payload: Literal { value: Bits.toBigInt bits }, length: (List.length groups + 1) * 5 }
  where
  groupP :: Parser Bit -> Parser BitString
  groupP prefixP = prefixP *> bitsP 4

payloadP opcode = do
  operation <- case opcode of
    0 -> pure Sum
    1 -> pure Product
    2 -> pure Minimum
    3 -> pure Maximum
    5 -> pure GreaterThan
    6 -> pure LessThan
    7 -> pure EqualTo
    _ -> fail $ "Unrecognized opcode: " <> show opcode
  lengthTypeID <- fromBinaryDigit
  case lengthTypeID of
    Bit false -> do
      totalSubpacketLength <- numP 15
      subPackets <- subPacketsUntilLength totalSubpacketLength
      unless (totalSubpacketLength == sum (_.length <$> subPackets))
        $ fail
        $ "Subpacket lengths do not sum to specified total length: "
        <> show totalSubpacketLength
      pure
        $ { payload: Operator { operation, subPackets: _.packet <$> subPackets }
          , length: totalSubpacketLength + 1 + 15
          }
    Bit true -> do
      numPackets <- numP 11
      subPackets <- replicate1A numPackets packetP
      pure
        $ { payload: Operator { operation, subPackets: _.packet <$> subPackets }
          , length: sum (_.length <$> subPackets) + 1 + 11
          }
  where
  subPacketsUntilLength :: Int -> Parser (NonEmptyList { packet :: Packet, length :: Int })
  subPacketsUntilLength l = do
    first <- packetP
    rest <- subPacketsUntilLengthR (l - first.length)
    pure $ NEList.cons' first rest
    where
    subPacketsUntilLengthR :: Int -> Parser (List { packet :: Packet, length :: Int })
    subPacketsUntilLengthR 0 = pure Nil

    subPacketsUntilLengthR l' = do
      packet <- packetP
      rest <- subPacketsUntilLengthR $ l' - packet.length
      pure $ packet : rest

inputP :: Parser Packet
inputP =
  _.packet
    <$> packetP
    <* many bit0P
    <* eof

run :: forall a. (Packet -> Either String a) -> String -> Either String a
run solver input = do
  bits <- runParser fromHexString input
  message <- runParser inputP $ showBinaryString bits
  solver message

part1 :: String -> Either String Int
part1 = run $ pure <<< sumVersions
  where
  sumVersions :: Packet -> Int
  sumVersions (Packet { version, payload: Literal _ }) = version

  sumVersions (Packet { version, payload: Operator { subPackets } }) = version + sum (sumVersions <$> subPackets)

part2 :: String -> Either String BigInt
part2 = run evaluate
  where
  evaluate :: Packet -> Either String BigInt
  evaluate (Packet { payload: Literal { value } }) = pure value

  evaluate (Packet { payload: Operator { operation: Sum, subPackets } }) = listOp sum subPackets

  evaluate (Packet { payload: Operator { operation: Product, subPackets } }) = listOp product subPackets

  evaluate (Packet { payload: Operator { operation: Minimum, subPackets } }) = listOp minimum subPackets

  evaluate (Packet { payload: Operator { operation: Maximum, subPackets } }) = listOp maximum subPackets

  evaluate (Packet { payload: Operator { operation: GreaterThan, subPackets: NonEmptyList (a :| b : Nil) } }) = booleanOp (>) a b

  evaluate (Packet { payload: Operator { operation: LessThan, subPackets: NonEmptyList (a :| b : Nil) } }) = booleanOp (<) a b

  evaluate (Packet { payload: Operator { operation: EqualTo, subPackets: NonEmptyList (a :| b : Nil) } }) = booleanOp (==) a b

  evaluate _ = Left "Invalid input: invalid packet"

  listOp :: (NonEmptyList BigInt -> BigInt) -> NonEmptyList Packet -> Either String BigInt
  listOp f ps = f <$> traverse evaluate ps

  booleanOp :: (BigInt -> BigInt -> Boolean) -> Packet -> Packet -> Either String BigInt
  booleanOp f a b = do
    a' <- evaluate a
    b' <- evaluate b
    pure $ if f a' b' then one else zero
