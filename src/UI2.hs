{-# LANGUAGE OverloadedStrings #-}
module UI2 where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
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

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Player1 | Player2 | Empty | Woods | HD

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
handleEvent g (AppEvent Tick)                       = nextS g (Cont (nextResult (g ^. hotdog)))
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
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
 $ vBox [ drawScore1 (g ^. score1)
        , drawScore2 (g ^. score2)
        , padTop (Pad 2) $ (drawGameOver1 (g ^. win2))
        , padTop (Pad 0) $ (drawGameOver2 (g ^. win1))
        ]

drawScore1 :: Int -> Widget Name
drawScore1 n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score 1")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawScore2 :: Int -> Widget Name
drawScore2 n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score 2")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

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
      | c == (getCoordinate (g ^. hotdog)) = HD
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Woods    = withAttr forestAttr $ str "💀 "
drawCell Empty     = withAttr emptyAttr $ str "🟫 "
drawCell HD        = withAttr hotdogAttr $ str "🔥 "
drawCell Player1   = withAttr player1Attr $ str "🎅 "
drawCell Player2   = withAttr player2Attr $ str "🤡 "




cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (player1Attr, V.black `on` V.black)
   ,(player2Attr, V.black `on` V.black)
   ,(emptyAttr, V.black `on` V.black)
   ,(forestAttr, V.brightCyan `on` V.brightCyan)
  , (gameOverAttr1, fg V.red `V.withStyle` V.bold)
  , (gameOverAttr2, fg V.red `V.withStyle` V.bold)
  , (hotdogAttr, V.black `on` V.black)
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


drawGameOver1 :: Bool  -> Widget Name
drawGameOver1 win2 =
  if win2 
     then withAttr gameOverAttr1 $ C.hCenter $ str "Player2 Win!"
     else emptyWidget

drawGameOver2 :: Bool  -> Widget Name
drawGameOver2 win1   =
  if win1 
     then withAttr gameOverAttr2 $ C.hCenter $ str "Player1 Win!"
  else emptyWidget

nextS :: Game -> Result Hotdog -> EventM Name (Next Game)
nextS s b = case next s b of
  Right s' -> continue =<< liftIO s'
  Left res -> continue (s { _gameover = False })

next :: Game -> Hotdog.Result Hotdog.Hotdog -> Either (Maybe Turn) (IO Game)
next s (Cont b') = Right (return (s { _hotdog = b'} ))


