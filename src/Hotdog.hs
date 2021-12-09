module Hotdog
    (
        Hotdog
    ,   Coordd
    ,   Result (..)
    ,   Turn (..)
    ,   initf
    ,   nextResult
    ,   getCoordinate
    ) where

import System.Random
import Linear.V2 (V2(..), _x, _y)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.IO.Unsafe  


data Hotdog = Hotdog
    {
        pos :: Coordd
    ,   dir :: Coordd
    ,   speed :: Int
    }

data Turn
  = P1
  | P2
  deriving (Eq, Show)

data Coordd = Coord
  { x :: Int 
  , y :: Int
  }
  deriving (Eq, Ord)

data Result a
  = Cont a
  | Score Turn
  deriving (Eq,  Show)
  
myheight, mywidth :: Int
myheight = 20
mywidth  = 20



addCoord :: Coordd -> Coordd -> Coordd
addCoord m n = Coord{x = (x m) + (x n), y = (y m) + (y n)}

mulCoord :: Coordd -> Int -> Coordd
mulCoord m c = Coord{ x = (x m) * c, y = (y m) * c}

moveDown :: Hotdog -> Hotdog
moveDown s = do
            let p = pos s
            let d = dir s
            let v = speed s
            s { pos = (addCoord p (mulCoord d v))}

nextResult :: Hotdog -> Hotdog
nextResult s = if (y (pos s)) > 0 then moveDown s
               else s

getCoordinate :: Hotdog -> V2 Int
getCoordinate b = V2 (x (pos b)) (y (pos b))

initf :: Hotdog
initf = Hotdog {   pos = Coord{ x=  unsafePerformIO (getStdRandom (randomR (0, mywidth-1))), y= fromIntegral myheight-1 }
                            , dir   = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = 1
            } 

initFood :: Turn -> IO Hotdog
initFood P1 = do
            t <- randomRIO(0,mywidth-1)
            return Hotdog {   pos = Coord{ x= fromIntegral t, y= fromIntegral myheight-1 }
                            , dir = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = 1
            }
