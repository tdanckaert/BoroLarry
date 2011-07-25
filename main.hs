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
rotate n d = let makePos d' = if d' >= 0
                              then d'
                              else d' + 4
             in makePos $ (d + n ) `mod` 4
--instance Eq Direction 
--where (Direction a b = 

data Robot = Robot { rcoords :: (Int,Int)
                   , direction :: Int}

-- Game state consists of list of robots and their positions.
data Game = Game { bots :: Var [Robot] }

robonames = ["spunky","bimbot"]
startpos = [(4,5), (7,8)]

moveBot vrobo n = do robo <- varGet vrobo
                     varSet vrobo ( move n robo)
                       where move n (Robot (x,y) d)
                               | d == 0 = (Robot (x,y+n) d) 
                               | d == 1 = (Robot (x+n,y) d)
                               | d == 2 = (Robot (x,y-n) d)
                               | d == 3 = (Robot (x-n,y) d)
                                          
rotateBot vrobo n = do (Robot pos dir) <- varGet vrobo
                       varSet vrobo (Robot pos (rotate n dir))
  {-| n > 0 = do (Robot pos dir) <- varGet vrobo
                               varSet vrobo (Robot pos (rotateLeft dir))
                  | n < 0 = do (Robot pos dir) <- varGet vrobo
                               varSet vrobo (Robot pos (rotateRight dir)) -}

makeCoord (x,y) = pt (x*40) (400 -(y*40))

game = do f <- frameFixed [text := "Boro Larry"]
          robo <- varCreate (Robot (4,5) 0)
          p <- panel f [on paint := drawGame [robo] ]
          set f [ layout := minsize (sz maxX maxY) $ widget p]
          set p [ on (charKey 'f') := (moveBot robo 1) >> repaint p
                , on (charKey 'b') := (moveBot robo (-1)) >> repaint p
                , on (charKey 'r') := (rotateBot robo 1) >> repaint p
                , on (charKey 't') := (rotateBot robo (-1)) >> repaint p]

-- From the names in 'robonames' generate a list of lists with their sprites:
-- [ [spunky0.png,spunky1.png,...,spunky3.png], [bimbot0.png, ....]]
loadBitmaps 
  = sequence ( do roboname <- robonames
                  return ( mapM (\x -> bitmapCreateFromFile ( "./img/" ++ roboname ++ (show x) ++ ".png")) [0..3]))
    
drawBot dc robo = let screencoords = (makeCoord (rcoords robo))
                  in do bs <- loadBitmaps
                        drawBitmap dc ((bs !! 0) !! (direction robo)) screencoords True []

bitmaps = do roboimg <- bitmapCreateFromFile("./Robo.png")
             return [roboimg]

drawGame vrobos dc viewArea 
  = do set dc [brushColor := black, brushKind := BrushSolid]
       tile <- bitmapCreateFromFile("./tile.png")
       mapM_ (\p -> drawBitmap dc tile p False []) coords
       sequence_ (do vrobo <- vrobos
                     return (do robo <- varGet vrobo
                                drawBot dc robo))
