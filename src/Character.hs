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
  , score1,score2
  , win1,win2,gameover
  , forest
  , hotdog
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Hotdog

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

--data Player = player1 | player2

data Game = Game
  { _d      :: Direction         -- ^ direction
  , _player1 :: Coord 
  , _player2  :: Coord    
  , _score1 :: Int
  , _score2 :: Int        
  , _win1 :: Bool            
  , _win2 :: Bool
  , _gameover :: Bool
  , _forest :: [Coord]
  , _hotdog :: Hotdog
  --, _woods :: [Coord]
  --, _unwalkable :: [Coord]
  } 


data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)


makeLenses '' Game


myheight, mywidth :: Int
myheight = 20
mywidth  = 20


trees :: [Coord]
trees = [(V2 0 11), (V2 0 12), (V2 1 12), (V2 1 11),(V2 2 11), (V2 2 10), (V2 3 10), (V2 3 11), (V2 4 11),              (V2 5 11), (V2 5 12),(V2 6 12), (V2 6 13),(V2 7 13),
        (V2 7 12), (V2 8 12), (V2 8 11), (V2 9 11), (V2 9 12), (V2 10 12), (V2 10 11), (V2 11 11), (V2 11 12), (V2 12 12), (V2 12 13),(V2 13 13), (V2 13 12),(V2 14 12),(V2 14 11),(V2 15 11),
        (V2 16 11), (V2 17 11), (V2 18 11), (V2 18 12),(V2 19 12), (V2 20 12)
        ]
        
  

initGame :: IO Game
initGame = do
  let x1 = (mywidth `div` 2) - 1
      y1 = (myheight `div` 3) - 1
      x2 = (mywidth `div` 2) + 1
      y2 = (myheight `div` 3) - 1
      g = Game
        {
          _d = South
        , _player1 = (V2 x1 y1)
        , _player2 = (V2 x2 y2)
        , _score1  = 10
        , _score2  = 10
        , _win1 = False
        , _win2 = False
        , _gameover = False
        , _forest   = trees
        , _hotdog = initf
        }
  return (execState initState g)

initState :: State Game ()
initState = do
  s <- get
  put s

-- check if play1 on border
outBorder1:: Game -> Direction ->  Bool
outBorder1 g North  = do
  let (V2 x y) = g ^. player1
  if y+1>=myheight then True
  else False
outBorder1 g South  = do
  let (V2 x y) = g ^. player1
  if y-1<=0 then True
  else False
outBorder1 g East  = do
  let (V2 x y) = g ^. player1
  if x+1>=mywidth then True
  else False
outBorder1 g West  = do
  let (V2 x y) = g ^. player1
  if x-1<=0 then True
  else False

  -- check if play2 on border
outBorder2:: Game -> Direction ->  Bool
outBorder2 g North  = do
  let (V2 x y) = g ^. player2
  if y+1>=myheight then True
  else False
outBorder2 g South  = do
  let (V2 x y) = g ^. player2
  if y-1<=0 then True
  else False
outBorder2 g East  = do
  let (V2 x y) = g ^. player2
  if x+1>=mywidth then True
  else False
outBorder2 g West  = do
  let (V2 x y) = g ^. player2
  if x-1<=0 then True
  else False

check :: Game -> Game
check g = do
  if g^. win1 == True then g & gameover %~ (\_ -> True)
  else if g^. win2 == True then g & gameover %~ (\_ -> True)
  else g

check_die :: Game -> Game
check_die g = do
  let (V2 x y) = g ^. player1
  let (V2 m n) = g ^. player2
  
  if g ^. player1 `elem` g ^. forest then g & win2 %~ (\_ -> True)
  else if g ^. player2 `elem` g ^. forest then g & win1 %~ (\_ -> True)
  else g

moves1 :: Direction -> Game -> Game
moves1 North g = do
  let (V2 x y) = g ^. player1
  let (V2 m n )= g ^. player2
  if x==m && y+1==n then g
  else if g ^. player1 `elem` trees then g & win2 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if y >= myheight-1 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 a (b+1))))

moves1 East g = do
  let (V2 x y) = g ^. player1
  let (V2 m n )= g ^. player2
  if y==n && x+1==m then g
  else if g ^. player1 `elem` trees then g & win2 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if x >= mywidth-1  then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 (a+1) b)))

moves1 West g = do
  let (V2 x y) = g ^. player1
  let (V2 m n )= g ^. player2
  if x==m+1 && y==n then g
  else if g ^. player1 `elem` trees then g & win2 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if x <= 0 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 (a-1) b)))

moves1 South g = do
  let (V2 x y) = g ^. player1
  let (V2 m n )= g ^. player2
  if x==m && y==n+1 then g
  else if g ^. player1 `elem` trees then g & win2 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if y <= 0 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 a (b-1))))



moves2 :: Direction -> Game -> Game
moves2 North g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if x==m && y+1==n then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if y >= myheight-1 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 a (b+1))))

moves2 East g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if y==n && x+1==m then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if x >= mywidth-1  then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 (a+1) b)))

moves2 West g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if x==m+1 && y==n then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if x <= 0 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 (a-1) b)))

moves2 South g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if x==m && y==n+1 then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. gameover == True then g
  else if y <= 0  then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 a (b-1))))




