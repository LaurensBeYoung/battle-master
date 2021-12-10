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
  , win1,win2,gameOver
  , forest,
    hotdog1,
    hotdog2,
    hotdog3,
    hotdog4,
    hotdog5,
    hotdog6,
    beef,
    beefs
    --d, gameOver, --win, --rock, --unwalkable
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
import Hotdog

myheight, mywidth :: Int
myheight = 20
mywidth  = 20
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
  , _forest :: [Coord]--, _woods :: [Coord]
  , _hotdog1 :: Hotdog
  , _hotdog2 :: Hotdog
  , _hotdog3 :: Hotdog
  , _hotdog4 :: Hotdog
  , _hotdog5 :: Hotdog
  , _hotdog6 :: Hotdog
  , _beef :: Coord 
  , _beefs :: Stream Coord      -- ^ the location of the player will be modified via I/O
  , _gameOver :: Bool            -- ^ the bool value mark the game is live or dead      
  --, _rock :: [Coord]
  --, _unwalkable :: [Coord]
  } 


data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data Stream a = Cons a (Stream a) deriving (Show,Eq, Ord)
infixr 5 `Cons`
infixr 5 <:>
(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons

head :: Stream a -> a
head (Cons x _ ) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs


makeLenses '' Game


fromList :: [a] -> Stream a
fromList = foldr Cons (error "Streams must be infinite")

trees :: [Coord]
trees = 
  [(V2 4 12),(V2 16 18),(V2 2 16),(V2 12 10),(V2 14 14)]++
        [(V2 0 7), (V2 0 8), (V2 1 8), (V2 1 7),(V2 2 7), (V2 2 6), (V2 3 7), (V2 5 7), (V2 5 8),(V2 6 8), (V2 6 9),(V2 7 9),
        (V2 7 8), (V2 8 8), (V2 8 7), 
        (V2 11 7), (V2 11 8), (V2 12 8), (V2 12 9),(V2 13 9), (V2 13 8),(V2 14 8),(V2 14 7),(V2 15 7),
        (V2 17 7), (V2 18 7), (V2 18 8),(V2 19 8), (V2 20 8)
        ]
        ++[(V2 11 3),(V2 10 3),(V2 9 3),(V2 8 3)]
        
notInTree :: Coord -> Stream Coord -> [Coord] -> Coord
notInTree s _ [] = s
notInTree s d t = if s `elem` t then notInTree (Character.head d) (Character.tail d) t
                  else s



initGame :: IO Game
initGame = do
  (Cons f fs) <-
    fromList . randomRs (V2 0 0, V2 (mywidth - 1) (myheight - 1)) <$> newStdGen
  let x1 = (mywidth `div` 2) - 1
      y1 = (myheight `div` 3) - 1
      x2 = (mywidth `div` 2) + 1
      y2 = (myheight `div` 3) - 1
      fd = notInTree f fs trees 
      g = Game
        {
          _d = South
        , _player1 = (V2 x1 y1)
        , _player2 = (V2 x2 y2)
        , _score1  = 5
        , _score2  = 5
        , _win1 = False
        , _win2 = False
        , _forest   = trees
        , _hotdog1 = initf1
        , _hotdog2 = initf2
        , _hotdog3 = initf3
        , _hotdog4 = initf4
        , _hotdog5 = initf5
        , _hotdog6 = initf6
        , _beef = fd
        , _beefs = fs
        , _gameOver = False
        }
  return (execState initState g)

-- getLoc :: Coord
-- getLoc = do 
--   if newLoc `elem` trees then getLoc
--   else newLoc
--     where newLoc = randomRs (V2 0 0, V2 (mywidth - 1) (myheight - 1))

-- initBeef ::  Game -> Game 
-- initBeef g = g & beef %~ (\_ -> getLoc)



getLoc :: Stream Coord -> [Coord] -> Game -> Coord
getLoc d t g = if (Character.head d) `elem` t then getLoc (Character.tail d) t g
             else if (Character.head d) == g^. beef then getLoc (Character.tail d) t g 
             else Character.head d

updateLoc :: Game -> Game
updateLoc g = do
  (g & beef %~ (\_ -> (getLoc (g ^. beefs) trees g))) & beefs %~ (\_ -> Character.tail (g ^. beefs))

initState :: State Game ()
initState = do
  (Cons f fs) <- use beefs
  beefs .= fs
  s <- get
  put s

checkWin1 :: Game -> Game
checkWin1 g = do 
  if g ^. score1>=12 then g & win1 %~ (\_ -> True)
  else g

checkWin2 :: Game -> Game
checkWin2 g = do 
  if g ^. score2>=12 then g & win2 %~ (\_ -> True)
  else g


check :: Game -> Game
check g = do
  if g^. win1 == True then g & gameOver %~ (\_ -> True)
  else if g^. win2 == True then g & gameOver %~ (\_ -> True)
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
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player1 == g ^. beef then  checkWin1 (updateLoc (g & score1 %~ (\x -> x+2))) 
  else if g ^. gameOver == True then g
  else if y >= myheight-1 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 a (b+1))))

moves1 East g = do
  let (V2 x y) = g ^. player1
  let (V2 m n )= g ^. player2
  if y==n && x+1==m then g
  else if g ^. player1 `elem` trees then g & win2 %~ (\_ -> True)
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player1 == g ^. beef then  checkWin1 (updateLoc (g & score1 %~ (\x -> x+2)))
  else if g ^. gameOver == True then g
  else if x >= mywidth-1  then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 (a+1) b)))

moves1 West g = do
  let (V2 x y) = g ^. player1
  let (V2 m n )= g ^. player2
  if x==m+1 && y==n then g
  else if g ^. player1 `elem` trees then g & win2 %~ (\_ -> True)
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player1 == g ^. beef then  checkWin1 (updateLoc (g & score1 %~ (\x -> x+2)))
  else if g ^. gameOver == True then g
  else if x <= 0 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 (a-1) b)))

moves1 South g = do
  let (V2 x y) = g ^. player1
  let (V2 m n )= g ^. player2
  if x==m && y==n+1 then g
  else if g ^. player1 `elem` trees then g & win2 %~ (\_ -> True)
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player1 == g ^. beef then  checkWin1 (updateLoc (g & score1 %~ (\x -> x+2)))
  else if g ^. gameOver == True then g
  else if y <= 0 then g
  else 
    check(check_die (g)) & (player1 %~ (\(V2 a b) -> (V2 a (b-1))))



moves2 :: Direction -> Game -> Game
moves2 North g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if x==m && y+1==n then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player2 == g ^. beef then  checkWin2 (updateLoc (g & score2 %~ (\x -> x+2)))
  else if g ^. gameOver == True then g
  else if y >= myheight-1 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 a (b+1))))

moves2 East g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if y==n && x+1==m then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player2 == g ^. beef then  checkWin2 (updateLoc (g & score2 %~ (\x -> x+2)))
  else if g ^. gameOver == True then g
  else if x >= mywidth-1  then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 (a+1) b)))

moves2 West g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if x==m+1 && y==n then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player2 == g ^. beef then  checkWin2 (updateLoc (g & score2 %~ (\x -> x+2)))
  else if g ^. gameOver == True then g
  else if x <= 0 then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 (a-1) b)))

moves2 South g = do
  let (V2 x y) = g ^. player2
  let (V2 m n )= g ^. player1
  if x==m && y==n+1 then g
  else if g ^. player2 `elem` trees then g & win1 %~ (\_ -> True)
  else if g ^. score1 == 0 then g & win2 %~ (\_ -> True)
  else if g ^. score2 == 0 then g & win1 %~ (\_ -> True)
  -- check if have taken the beef
  else if g ^. player2 == g ^. beef then checkWin2 (updateLoc (g & score2 %~ (\x -> x+2))) 
  else if g ^. gameOver == True then g
  else if y <= 0  then g
  else 
    check(check_die (g)) & (player2 %~ (\(V2 a b) -> (V2 a (b-1))))


