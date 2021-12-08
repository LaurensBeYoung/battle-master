{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Character
  ( moves1,moves2
  , initGame
  , Game(..)
  , Direction(..)
  , myheight, mywidth
  , player1,player2 --d, gameOver, --win, --rock, --unwalkable
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen, getStdRandom)
import System.IO.Unsafe

type Coord = V2 Int


data Game = Game
  { _d      :: Direction         -- ^ direction
  , _player1 :: Coord 
  , _player2  :: Coord            -- ^ the location of the player will be modified via I/O
  , _gameOver :: Bool            -- ^ the bool value mark the game is live or dead      
  , _win :: Bool
  --, _rock :: [Coord]
  --, _unwalkable :: [Coord]
  } deriving (Show)


data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)


makeLenses '' Game


myheight, mywidth :: Int
myheight = 10
mywidth  = 10

initGame :: IO Game
initGame = do
  let x1= 3
      y1 = 2
      x2 = 6
      y2 = 2
      g = Game
        {
          _d = South
        , _player1 = (V2 x1 y1)
        , _player2 = (V2 x2 y2)
        , _gameOver = False
        , _win = False
        }
  return (execState initState g)

initState :: State Game ()
initState = do
  s <- get
  put s


check :: Game -> Game
check g = do
  g

check_die :: Game -> Game
check_die g = do
  if g ^. win == True then g & gameOver %~ (\_ -> True)
  else g

moves1 :: Direction -> Game -> Game
moves1 North g = do
  let (V2 x y) = g ^. player1
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 a (b+1))))

moves1 East g = do
  let (V2 x y) = g ^. player1
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 (a+1) b)))

moves1 West g = do
  let (V2 x y) = g ^. player1
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 (a-1) b)))

moves1 South g = do
  let (V2 x y) = g ^. player1
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 a (b-1))))



moves2 :: Direction -> Game -> Game
moves2 North g = do
  let (V2 x y) = g ^. player2
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 a (b-1))))

moves2 East g = do
  let (V2 x y) = g ^. player2
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 (a+1) b)))

moves2 West g = do
  let (V2 x y) = g ^. player2
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 (a-1) b)))

moves2 South g = do
  let (V2 x y) = g ^. player2
  if g ^. win == True then g
  else if g ^. gameOver == True then g
  else if y >= myheight - 1 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 a (b-1))))

