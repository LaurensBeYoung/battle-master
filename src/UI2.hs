{-# LANGUAGE OverloadedStrings #-}
module UI2 where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import Control.Lens hiding ((<|), (|>), (:>), (:<), Empty)

import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Character
import Hotdog

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- data UI = UI
--   { _game    :: Game         -- ^ tetris game
--   , _paused  :: Bool         -- ^ game paused
--   }

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Player1 | Player2 | Empty | Woods | HD | BF

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

  -- ui <- customMain initialVty builder (Just chan) app $ UI
  --   { _game   = g
  --   , _paused  = False  
  --   }
  -- return $ ui ^. game
--  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = nextS (checkHit1 $ checkHit2 g)(Cont [(nextResult (g ^. win1) (g ^. win2) (g ^. hotdog1)),(nextResult (g ^. win1) (g ^. win2) (g ^. hotdog2)),(nextResult (g ^. win1) (g ^. win2) (g ^. hotdog3)),(nextResult (g ^. win1) (g ^. win2) (g ^. hotdog4)),(nextResult (g ^. win1) (g ^. win2) (g ^. hotdog5))]) 
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ moves2 North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ moves2 South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ moves2 East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moves2 West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ moves1 North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ moves1 South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ moves1 East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ moves1 West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g
-- handleEvent ui (VtyEvent (V.EvKey (V.KChar 'p') [])) =
--   guarded
--     (not . view locked)
--     (over paused not)
--     ui

-- guarded :: (UI -> Bool) -> (UI -> UI) -> UI -> EventM Name (Next UI)
-- guarded p f ui = continue
--   $ if not (p ui) || ui ^. g . to win1 || ui ^. g . to win2 || ui ^. g . to score1 <=0 || ui ^. g . to score2<=0
--     then ui
--     else f ui

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
 $ vBox [ drawScore1 (g ^. score1)
        , drawScore2 (g ^. score2)
        , padTop (Pad 2) $ (drawGameOver1 g)
        , padTop (Pad 0) $ (drawGameOver2 g)
        ]

drawScore1 :: Int -> Widget Name
drawScore1 n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score P1")
  $ C.hCenter
  $ padAll 1
  $ str $ show (noMinus n)

drawScore2 :: Int -> Widget Name
drawScore2 n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score P2")
  $ C.hCenter
  $ padAll 1
  $ str $ show (noMinus n)

noMinus :: Int -> Int 
noMinus n = if n<= 0 then 0
  else if n >=10 then 10
  else
    n

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Medieval Battle")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [myheight-1,myheight-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..mywidth-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == g ^. player1 = Player1
      | c == g ^. player2 = Player2
      | c `elem` (g ^. forest)  = Woods 
      | c == (getCoordinate (g ^. hotdog1)) = HD
      | c == (getCoordinate (g ^. hotdog2)) = HD
      | c == (getCoordinate (g ^. hotdog3)) = HD
      | c == (getCoordinate (g ^. hotdog4)) = HD
      | c == (getCoordinate (g ^. hotdog5)) = HD  
      -- | c == (getCoordinate (g ^. hotdog6)) = HD    
      | c == g ^. beef = BF
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Woods    = withAttr forestAttr $ str "💀 "
drawCell Empty     = withAttr emptyAttr $ str "🟫 "
drawCell HD        = withAttr hotdogAttr $ str "🔥 "
drawCell BF        = withAttr beefAttr $ str "💰 "
drawCell Player1   = withAttr player1Attr $ str "🎅 "
drawCell Player2   = withAttr player2Attr $ str "🤡 "
--drawCell Woods     = withAttr forestAttr cw


-- drawCell HDs = withAttr hotdogsAttr cw


cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (player1Attr, V.black `on` V.black)
   ,(player2Attr, V.black `on` V.black)
  -- ,(emptyAttr, V.black `on` V.black)
   ,(forestAttr, V.brightCyan `on` V.brightCyan)
  , (gameOverAttr1, fg V.red `V.withStyle` V.bold)
  , (gameOverAttr2, fg V.red `V.withStyle` V.bold)
  --q, (hotdogAttr, V.red `on` V.brightCyan)
  --, (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]
 
gameOverAttr1, gameOverAttr2:: AttrName
gameOverAttr1 = "Player2 Win!"
gameOverAttr2 = "Player1 Win!"

player1Attr, player2Attr, emptyAttr, forestAttr, hotdogAttr:: AttrName
player1Attr = "player1Attr"
player2Attr = "player2Attr"
emptyAttr = "emptyAttr"
forestAttr = "forestAttr"
hotdogAttr = "hotdogAttr"
beefAttr = "beefAttr"


drawGameOver1 :: Game -> Widget Name
drawGameOver1 game =
  if (changeWin2 game) ^. win2
     then withAttr gameOverAttr1 $ C.hCenter $ str "Player2 Win!"
     else emptyWidget

drawGameOver2 :: Game -> Widget Name
drawGameOver2 game  = 
  if (changeWin1 game) ^. win1  
     then withAttr gameOverAttr2 $ C.hCenter $ str "Player1 Win!"
  else emptyWidget

changeWin1 :: Game -> Game
changeWin1 game = 
  if game ^. score2<=0
    then  game & win1 %~ (\_ -> True)
  else if game ^. score1>=12
    then game & win1 %~ (\_ -> True)
  else
    game

changeWin2 :: Game -> Game
changeWin2 game = 
  if game ^. score1<=0
    then game & win2 %~ (\_ -> True)
  else if game ^. score2>=12
    then game & win2 %~ (\_ -> True)
  else
    game

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

nextS :: Game -> Result [Hotdog] -> EventM Name (Next Game)
nextS s b = case next s b of
  Right s' -> continue =<< liftIO s'
  Left res -> continue (s { _gameOver = False })

next :: Game -> Hotdog.Result [Hotdog.Hotdog] -> Either (Maybe Turn) (IO Game)
next s (Cont [b1,b2,b3,b4,b5]) = Right (return (s { _hotdog1 = b1, _hotdog2 = b2, _hotdog3 = b3, _hotdog4 = b4, _hotdog5 = b5} ))


checkHit1 :: Game -> Game
checkHit1 g = do 
  if (g ^. player1) == (getCoordinate $ g ^. hotdog1) then g & (score1 %~ (\x -> x-1))
  else if (g ^. player1) == (getCoordinate $ g ^. hotdog2) then g & (score1 %~ (\x -> x-1))
  else if (g ^. player1) == (getCoordinate $ g ^. hotdog3) then g & (score1 %~ (\x -> x-1))
  else if (g ^. player1) == (getCoordinate $ g ^. hotdog4) then g & (score1 %~ (\x -> x-1))
  else if (g ^. player1) == (getCoordinate $ g ^. hotdog5) then g & (score1 %~ (\x -> x-1))
  -- else if (g ^. player1) == (getCoordinate $ g ^. hotdog6) then g & (score1 %~ (\x -> x-1))
  else
    g

checkHit2 :: Game -> Game
checkHit2 g = do 
  if (g ^. player2) == (getCoordinate $ g ^. hotdog1) then g & (score2 %~ (\x -> x-1))
  else if (g ^. player2) == (getCoordinate $ g ^. hotdog2) then g & (score2 %~ (\x -> x-1))
  else if (g ^. player2) == (getCoordinate $ g ^. hotdog3) then g & (score2 %~ (\x -> x-1))
  else if (g ^. player2) == (getCoordinate $ g ^. hotdog4) then g & (score2 %~ (\x -> x-1))
  else if (g ^. player2) == (getCoordinate $ g ^. hotdog5) then g & (score2 %~ (\x -> x-1))
  -- else if (g ^. player1) == (getCoordinate $ g ^. hotdog6) then g & (score2 %~ (\x -> x-1))
  else
    g


  



