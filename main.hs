import Graphics.UI.WX
import Graphics.UI.WXCore.Image
import Data.List
import IO

radius, maxX,maxY :: Int
maxX = 400
maxY = 400
radius = 10

maxH:: Int
maxH = maxY - radius

myRect = rectBetween (pt 10 10) (pt 30 30)

coords = [ (pt x y) | x <-[0,40..400], y<-[0,40..400]]

main = start game


-- Direction is represented as the numbers 0,1,2,3,
-- which stand for N,E,S,W respectively
rotateRight d = (d+1) `mod` 4
rotateLeft d = (d+3) `mod` 4
--instance Eq Direction 
--where (Direction a b = 

data Robot = Robot { rcoords :: (Int,Int)
                   , direction :: Int}

-- Game state consists of list of robots and their positions.
data Game = Game { bots :: Var [Robot] }

moveBot vrobo n = do robo <- varGet vrobo
                     varSet vrobo ( move n robo)
                       where move n (Robot (x,y) d)
                               | d == 0 = (Robot (x,y+n) d) 
                               | d == 1 = (Robot (x+n,y) d)
                               | d == 2 = (Robot (x,y-n) d)
                               | d == 3 = (Robot (x-n,y) d)
                                          
rotateBot vrobo n = do (Robot pos dir) <- varGet vrobo
                       varSet vrobo (Robot pos (rotateLeft dir))

makeCoord (x,y) = pt (x*40) (400 -(y*40))

game = do f <- frameFixed [text := "Boro Larry"]
          vrobo <- varCreate (Robot (4,5) 0)
          p <- panel f [on paint := drawGame vrobo]
          set f [ layout := minsize (sz maxX maxY) $ widget p]
          set p [ on (charKey 'f') := (moveBot vrobo 1) >> repaint p 
                , on (charKey 'r') := rotateBot vrobo 1]

drawGame vrobo dc viewArea
  = do set dc [brushColor := black, brushKind := BrushSolid]
       robo <- varGet vrobo
       tile <- bitmapCreateFromFile("./tile.png")
       mapM_ (\p -> drawBitmap dc tile p False []) coords
       roboimg <- bitmapCreateFromFile("./Robo.png")
       drawBitmap dc roboimg (makeCoord (rcoords robo)) True []
          
hello :: IO ()
hello = do f <- frame [text := "Hi!"]
           quit <- button f [text := "Tschüß", on command := close f]
           set f [layout := margin 10 (column 5 [floatCentre (label "QuatschSoft 2011")
                                                ,floatCentre (widget quit)])]