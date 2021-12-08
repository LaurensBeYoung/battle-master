{-# LANGUAGE OverloadedStrings #-}
module UI2 where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Character

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

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Player1 | Player2 | Empty

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

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
--handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ moves1 North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ moves1 South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ moves1 East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moves1 West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ moves2 North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ moves2 South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ moves2 East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ moves2 West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawScore 0) <+> drawGrid g ]

--drawStats :: Game -> Widget Name
--drawStats g = hLimit 11
--  $ vBox [ drawScore (g ^. score)
--         , padTop (Pad 2) $ drawGameOver (g ^. dead)
--         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [myheight-1,myheight-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..mywidth-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == g ^. player1 = Player1
      | c == g ^. player2 = Player2
    --  | c == g ^. food      = Food
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Empty   = withAttr emptyAttr cw
drawCell Player1   = withAttr player1Attr cw
drawCell Player2   = withAttr player2Attr cw



cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (player1Attr, V.blue `on` V.blue)
   ,(player2Attr, V.white `on` V.white)
   ,(emptyAttr, V.red `on` V.red)
  --, (foodAttr, V.red `on` V.red)
  --, (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

player1Attr, player2Attr, emptyAttr :: AttrName
player1Attr = "player1Attr"
player2Attr = "player2Attr"
emptyAttr = "emptyAttr"
