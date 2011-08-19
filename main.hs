import Graphics.UI.WX
import Graphics.UI.WXCore.Image
import Data.List
import Control.Monad
import Control.Concurrent.Thread.Delay
import IO

maxX = 400
maxY = 400

coords = [ (pt x y) | x <-[0,40..400], y<-[0,40..400]]

main = start game

-- Direction is represented as the numbers 0,1,2,3,
-- which stand for N,E,S,W respectively
-- rotation of n*90 degrees is then given by adding n mod 4.
rotate n d = let makePos d' = if d' >= 0
                              then d'
                              else d' + 4
             in makePos $ (d + n ) `mod` 4

data Robot = Robot { rcoords :: (Int,Int)
                   , direction :: Int}


robonames = ["spunky","bimbot","bimbot"]
startpos = [(4,5), (7,8), (6,8)]

{- 
moveBot: take the current list of Robots, and an order to move one of them,
check if the target square is free, if not, try to move the robot in the target square as well, and then move
-}
moveBot i n robos 
    | abs(n) ==  1 = let (Robot pos d) = robos !! i
                         move (x,y) -- translate robot movement order into a movement in a particular direction on the board
                           | d == 0 = (x,y+n)
                           | d == 1 = (x+n,y)
                           | d == 2 = (x,y-n)
                           | d == 3 = (x-n,y)
                     in tryMove robos i move
    | n > 1 = moveBot i (n-1) (moveBot i 1 robos)
    | n <(-1) = moveBot i (n+1) (moveBot i (-1) robos) -- probably don't need this case

                       
tryMove robos i move = let (Robot pos d) = robos !! i
                           target = move pos
                           robos' = (take i robos)
                                    ++ (Robot target d) : drop (1+i) robos
                       in case (roboAt target robos) of
                         Nothing -> robos'
                         Just j -> let robos'' = (tryMove robos j move)
                                   in case (roboAt target robos'') of
                                     Nothing -> tryMove robos'' i move
                                     Just k -> robos''
                                      
roboAt target = findIndex ( \(Robot pos d) -> pos == target)
                                          
rotateBot i n robos = let (Robot pos direction) = robos !! i
                          robo' = (Robot pos (rotate n direction))
                      in (take i robos) ++ robo' : (drop (1+i) robos)
                   
programSteps = [("move 1", moveBot 0 1)
               ,("move 2", moveBot 0 2)
               ,("move 3", moveBot 0 3)
               ,("turn left", rotateBot 0 (-1))
               ,("turn right", rotateBot 0 1)
               ,("back up", moveBot 0 (-1))]
            
makeCoord (x,y) = pt (x*40) (400 -(y*40))

setProgram vProgram choices = let getProgram = mapM 
                                               (\c -> (do s <- get c selection
                                                          return (snd (programSteps !! s))))
                                               choices
                              in do p <- getProgram
                                    varSet vProgram p

game = do f <- frameFixed [text := "Boro Larry"]
          vrobos <- varCreate $ map (\p -> (Robot p 0)) startpos
          vProgram <- varCreate myProgram
          sprites <- loadBitmaps
          tile <- bitmapCreateFromFile( "./tile.png")
          p <- panel f [on paint := drawGame vrobos sprites tile]
          programSlots <- replicateM 5 (choice f  [items := map fst programSteps])
          someInt <- varCreate 1
          movetimer <- timer f [interval := 500, enabled := False]
          goButton <- button f [text:= "Go!", on command:= 
                                              do setProgram vProgram programSlots
                                                 runProgram vProgram vrobos f p movetimer ]
          set f [ layout :=  
                  (column 5 [floatCentre $ minsize (sz maxX maxY) $ (widget p)
                            ,floatCentre (row 3 (map widget programSlots)) 
                            ,alignLeft (widget goButton)])]
          set p [ on (charKey 'f') := (varUpdate vrobos (moveBot 0 1)) >> repaint p
                , on (charKey 'b') := (varUpdate vrobos (moveBot 0 (-1))) >> repaint p
                , on (charKey 't') := (varUpdate vrobos (rotateBot 0 1)) >> repaint p
                , on (charKey 'r') := (varUpdate vrobos (rotateBot 0 (-1))) >> repaint p
                , on (charKey 'g') := (runProgram vProgram vrobos f p movetimer)]

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

myProgram = [moveBot 0 3, rotateBot 0 1, moveBot 0 2, rotateBot 0 1, moveBot 0 1]

runProgram vProgram vrobos f p t = set t [ on command := stepProgram t >> repaint p
                                         , enabled :~ not]
  where stepProgram t = do program <- varGet vProgram
                           if (length program >= 1)
                             then do varUpdate vrobos (program !! 0)
                                     varSet vProgram (tail program)
                             else set t [enabled :~ not]
