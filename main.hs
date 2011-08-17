import Graphics.UI.WX
import Graphics.UI.WXCore.Image
import Data.List
import Control.Monad
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

data Robot = Robot { rcoords :: (Int,Int)
                   , direction :: Int}

robonames = ["spunky","bimbot","bimbot"]
startpos = [(4,5), (7,8), (6,8)]

moveBot vrobos i n = do robos <- varGet vrobos
                        varSet vrobos (tryMove robos i n)
                        
{- 
tryMove: take the current list of Robots, and an order to move one of them,
check if the target square is free, if not, try to move the robot in the target square as well, and then move
-}
tryMove robos i n = let (Robot pos d) = robos !! i
                        move (x,y) -- translate robot movement order into a movement in a particular direction on the board
                          | d == 0 = (x,y+n)
                          | d == 1 = (x+n,y)
                          | d == 2 = (x,y-n)
                          | d == 3 = (x-n,y)
                    in tryMove' robos i move
                       
tryMove' robos i move = let (Robot pos d) = robos !! i
                            target = move pos
                            robos' = (take i robos)
                                     ++ (Robot target d) : drop (1+i) robos
                        in case (roboAt target robos) of
                          Nothing -> robos'
                          Just j -> let robos'' = (tryMove' robos j move)
                                    in case (roboAt target robos'') of
                                      Nothing -> tryMove' robos'' i move
                                      Just k -> robos''
                                      
data Programstep = Rotate Int Int Int -- robot number, steps, priority
                 | Move Int Int Int -- robot number, steps, priority
                   
myProgram = [Move 0 3 800, Rotate 0 1 300, Move 0 2 650, Rotate 0 1 200, Move 0 1 250]
                   
execute (Move i n k) vrobos = moveBot vrobos i n
execute (Rotate i n k) vrobos = rotateBot vrobos i n
                                    
roboAt target = findIndex ( \(Robot pos d) -> pos == target)
                                          
rotateBot vrobos i n = do robos <- varGet vrobos
                          let (Robot pos direction) = robos !! i
                              robo' = (Robot pos (rotate n direction))
                              robos' = (take i robos) ++ robo' : (drop (1+i) robos)
                            in varSet vrobos robos'

makeCoord (x,y) = pt (x*40) (400 -(y*40))

game = do f <- frameFixed [text := "Boro Larry"]
          vrobos <- varCreate $ map (\p -> (Robot p 0)) startpos
          vProgram <- varCreate myProgram
          sprites <- loadBitmaps
          tile <- bitmapCreateFromFile( "./tile.png")
          p <- panel f [on paint := drawGame vrobos sprites tile]
          set f [ layout := minsize (sz maxX maxY) $ widget p]
          set p [ on (charKey 'f') := (moveBot vrobos 0 1) >> repaint p
                , on (charKey 'b') := (moveBot vrobos 0 (-1)) >> repaint p
                , on (charKey 't') := (rotateBot vrobos 0 1) >> repaint p
                , on (charKey 'r') := (rotateBot vrobos 0 (-1)) >> repaint p
                , on (charKey 'g') := (runProgram vProgram vrobos f p)]

runProgram vProgram vrobos f p = do t <- timer f [interval := 1000, on command := stepProgram >> repaint p]
                                    return ()
                                    where stepProgram = do program <- varGet vProgram
                                                           execute (program !! 0) vrobos
                                                           varSet vProgram (tail program)

-- From the names in 'robonames' generate a list of lists with their sprites:
-- [ [spunky0.png,spunky1.png,...,spunky3.png], [bimbot0.png, ....]]
loadBitmaps 
  = sequence ( do roboname <- robonames
                  return ( mapM (\x -> bitmapCreateFromFile ( "./img/" ++ roboname ++ (show x) ++ ".png")) [0..3]))
    
drawBot' dc robo sprites = let screencoords = (makeCoord  (rcoords robo))
                           in drawBitmap dc (sprites !! (direction robo)) screencoords True []

drawGame vrobos sprites tile dc viewArea
  = do set dc [brushColor := black, brushKind := BrushSolid]
       mapM_ (\p -> drawBitmap dc tile p False []) coords
       robos <- varGet vrobos
       zipWithM_ (drawBot' dc) robos sprites 
