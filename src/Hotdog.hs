module Hotdog
    (
        Hotdog
    ,   Coordd
    ,   Result (..)
    ,   Turn (..)
    ,   initf1
    ,   initf2
    ,   initf3
    ,   initf4
    ,   initf5
    ,   initf6
    ,   nextResult
    ,   getCoordinate
    
    ) where

import System.Random
import Linear.V2 (V2(..), _x, _y)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.IO.Unsafe  


myheight, mywidth :: Int
myheight = 20
mywidth  = 20

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


nextResult :: Bool -> Bool -> Hotdog -> Hotdog
nextResult win1 win2 s = 
               if win1==True then do s
               else if win2==True then do s
               else if (speed s) == 0 then do s { pos = Coord{ x=  (x (pos s)), y= fromIntegral myheight-1 },
                            speed = (speed s) + 1}
               else if (speed s) >= 3 then do s { pos = Coord{ x=  (x (pos s))+ unsafePerformIO (getStdRandom (randomR (-3, 2))), y= fromIntegral myheight-1 },
                            speed =  1}
               else if ((y (pos s))-(speed s)) > 0 then moveDown s
               else if (y (pos s)) > 0 then Hotdog { pos = (addCoord (pos s) Coord{x=0, y= -1}), dir = (dir s), speed = (speed s) }
               else do s { pos = Coord{ x=  (x (pos s)) + unsafePerformIO (getStdRandom (randomR (-2, 2))), y= fromIntegral myheight-1 },
                            speed = (speed s) + unsafePerformIO (getStdRandom (randomR (-1, 1)))}

-- nextResults :: [Hotdog] -> [Hotdog]
-- nextResults [] = []
-- nextResults (d:ds) = [nextResult d] ++ (nextResults ds)

getCoordinate :: Hotdog -> V2 Int
getCoordinate b = V2 (x (pos b)) (y (pos b))


initf1 :: Hotdog
initf1 = Hotdog {   pos = Coord{ x=  unsafePerformIO (getStdRandom (randomR (0, mywidth-9))), y= fromIntegral myheight-1 }
                            , dir = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = unsafePerformIO (getStdRandom (randomR (1, 4)))
            } 

initf2 :: Hotdog
initf2 = Hotdog {   pos = Coord{ x=  unsafePerformIO (getStdRandom (randomR (0, mywidth-3))), y= fromIntegral myheight-1 }
                            , dir = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = unsafePerformIO (getStdRandom (randomR (2, 3)))
            } 
initf3 :: Hotdog
initf3 = Hotdog {   pos = Coord{ x=  unsafePerformIO (getStdRandom (randomR (0, mywidth-1))), y= fromIntegral myheight-1 }
                            , dir = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = unsafePerformIO (getStdRandom (randomR (1, 3)))
            } 
initf4 :: Hotdog
initf4 = Hotdog {   pos = Coord{ x=  unsafePerformIO (getStdRandom (randomR (2, mywidth-1))), y= fromIntegral myheight-1 }
                            , dir = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = unsafePerformIO (getStdRandom (randomR (2, 4)))
            } 
initf5 :: Hotdog
initf5 = Hotdog {   pos = Coord{ x=  unsafePerformIO (getStdRandom (randomR (7, mywidth-1))), y= fromIntegral myheight-1 }
                            , dir = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = unsafePerformIO (getStdRandom (randomR (3, 5)))
            } 

initf6 :: Hotdog
initf6 = Hotdog {   pos = Coord{ x=  unsafePerformIO (getStdRandom (randomR (2, mywidth-7))), y= fromIntegral myheight-1 }
                            , dir = Coord{ x= fromIntegral 0, y=  -1 }
                            , speed = unsafePerformIO (getStdRandom (randomR (1, 5)))
            } 















