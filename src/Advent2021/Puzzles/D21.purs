module Advent2021.Puzzles.D21
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, evalState, get, put)
import Data.Either (Either)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof, string)
import Text.Parsing.StringParser.Combinators (optional)

inputP :: Parser { p1 :: Int, p2 :: Int }
inputP = do
  _ <- string "Player 1 starting position: "
  p1 <- integer
  newline
  _ <- string "Player 2 starting position: "
  p2 <- integer
  optional newline
  eof
  pure { p1, p2 }

data Player
  = P1
  | P2

type PlayerState
  = { position :: Int
    , score :: Int
    }

type Game
  = { player1 :: PlayerState
    , player2 :: PlayerState
    , toPlay :: Player
    }

createGame :: { p1 :: Int, p2 :: Int } -> Game
createGame { p1, p2 } =
  { toPlay: P1
  , player1: { position: p1, score: 0 }
  , player2: { position: p2, score: 0 }
  }

deterministicDice :: forall m. MonadState { nextRoll :: Int } m => m Int
deterministicDice = do
  { nextRoll } <- get
  put $ { nextRoll: if nextRoll == 100 then 1 else nextRoll + 1 }
  pure nextRoll

turn :: forall m. Monad m => Game -> m Int -> m Game
turn game@{ toPlay } dice = do
  let
    { selectPlayer, updatePlayer, toPlay' } = case toPlay of
      P1 ->
        { selectPlayer: _.player1
        , updatePlayer: game { player1 = _ }
        , toPlay': P2
        }
      P2 ->
        { selectPlayer: _.player2
        , updatePlayer: game { player2 = _ }
        , toPlay': P1
        }

    { position, score } = selectPlayer game
  position' <- playerTurn position
  pure $ (updatePlayer { position: position', score: score + position' }) { toPlay = toPlay' }
  where
  playerTurn :: Int -> m Int
  playerTurn position = do
    roll1 <- dice
    roll2 <- dice
    roll3 <- dice
    pure $ ((position + roll1 + roll2 + roll3 - 1) `mod` 10) + 1

type GameStep
  = { step :: Game, turns :: Int }

play :: forall m. MonadRec m => m Int -> Game -> m GameStep
play dice initial = tailRecM resultR { step: initial, turns: 0 }
  where
  resultR :: GameStep -> m (Step GameStep GameStep)
  resultR { step: step@{ player1: { score: s1 }, player2: { score: s2 } }, turns } = do
    if s1 >= 1000 || s2 >= 1000 then
      pure $ Done { step, turns: turns }
    else do
      step' <- turn step dice
      pure $ Loop { step: step', turns: turns + 1 }

part1 :: String -> Either String Int
part1 input = do
  positions <- runParser inputP input
  let
    { step: { player1: { score: s1 }, player2: { score: s2 }, toPlay }, turns } =
      evalState
        (play deterministicDice $ createGame positions)
        { nextRoll: 1 }

    diceRolls = turns * 3

    losingScore = case toPlay of
      P1 -> s1
      P2 -> s2
  pure $ losingScore * diceRolls

part2 :: String -> Either String Int
part2 input = pure 0
