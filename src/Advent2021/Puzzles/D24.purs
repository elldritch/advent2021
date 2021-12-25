module Advent2021.Puzzles.D24
  ( Instruction(..)
  , Operand(..)
  , Program
  , Registers
  , Variable(..)
  , inputP
  , part1
  , part2
  , run
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser, token)
import Control.Alternative ((<|>))
import Data.Either (Either(..), note)
import Data.Foldable (intercalate)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Lazy as ListL
import Data.Maybe (Maybe(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof, string)
import Text.Parsing.StringParser.Combinators (sepEndBy)

data Variable
  = W
  | X
  | Y
  | Z

variableP :: Parser Variable
variableP =
  (char 'w' $> W)
    <|> (char 'x' $> X)
    <|> (char 'y' $> Y)
    <|> (char 'z' $> Z)

type Registers
  = { w :: Int
    , y :: Int
    , x :: Int
    , z :: Int
    }

initialRegisters :: Registers
initialRegisters = { w: 0, y: 0, x: 0, z: 0 }

set :: Variable -> Int -> Registers -> Registers
set v i rs = case v of
  W -> rs { w = i }
  X -> rs { x = i }
  Y -> rs { y = i }
  Z -> rs { z = i }

data Operand
  = Var Variable
  | Literal Int

operandP :: Parser Operand
operandP = (Var <$> variableP) <|> (Literal <$> integer)

get :: Operand -> Registers -> Int
get (Literal n) _ = n

get (Var v) rs = case v of
  W -> rs.w
  X -> rs.x
  Y -> rs.y
  Z -> rs.z

data Instruction
  = Input Variable
  | Add Variable Operand
  | Mul Variable Operand
  | Div Variable Operand
  | Mod Variable Operand
  | Eql Variable Operand

instructionP :: Parser Instruction
instructionP =
  (Input <$> (string "inp " *> variableP))
    <|> operationP "add" Add
    <|> operationP "mul" Mul
    <|> operationP "div" Div
    <|> operationP "mod" Mod
    <|> operationP "eql" Eql
  where
  operationP :: String -> (Variable -> Operand -> Instruction) -> Parser Instruction
  operationP name ctor = token (string name) *> (ctor <$> token variableP <*> operandP)

type Program
  = List Instruction

inputP :: Parser Program
inputP = sepEndBy instructionP newline <* eof

run :: Program -> List Int -> Either String Registers
run = runR initialRegisters
  where
  runR :: Registers -> Program -> List Int -> Either String Registers
  runR registers (instruction : instructions) inputs' = case instruction of
    Input v -> case List.uncons inputs' of
      Just { head, tail } -> runR (set v head registers) instructions tail
      Nothing -> Left "Invalid input: out of inputs"
    Add v o -> operate (+) v o
    Mul v o -> operate (*) v o
    Div v o -> operate (/) v o
    Mod v o -> operate mod v o
    Eql v o -> operate (\a b -> if a == b then 1 else 0) v o
    where
    operate :: (Int -> Int -> Int) -> Variable -> Operand -> Either String Registers
    operate f v o = runR (set v (f (get (Var v) registers) (get o registers)) registers) instructions inputs'

  runR registers Nil _ = pure registers

part1 :: String -> Either String String
part1 input = do
  program <- runParser inputP input
  -- let
  --   _ = spyW' "modelNumberDigits" (\ds -> intercalate "\n" $ map (intercalate "") $ map (map show) ds) $ ListL.take 20 modelNumberDigits
  highestValidModelNumber <-
    note "Invalid input: no valid model numbers"
      $ ListL.head
      $ ListL.dropWhile
          ( \digits -> case run program digits of
              Right { z } -> z /= 0
              Left _ -> true
          )
          modelNumberDigits
  pure $ intercalate "" $ map show highestValidModelNumber
  where
  modelNumberDigits :: ListL.List (List Int)
  modelNumberDigits = do
    d1 <- ListL.range 9 1
    d2 <- ListL.range 9 1
    d3 <- ListL.range 9 1
    d4 <- ListL.range 9 1
    d5 <- ListL.range 9 1
    d6 <- ListL.range 9 1
    d7 <- ListL.range 9 1
    d8 <- ListL.range 9 1
    d9 <- ListL.range 9 1
    d10 <- ListL.range 9 1
    d11 <- ListL.range 9 1
    d12 <- ListL.range 9 1
    d13 <- ListL.range 9 1
    d14 <- ListL.range 9 1
    pure $ d1 : d2 : d3 : d4 : d5 : d6 : d7 : d8 : d9 : d10 : d11 : d12 : d13 : d14 : Nil

part2 :: String -> Either String Int
part2 input = pure 0
