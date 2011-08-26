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
moveBot n i: robot number i moves n steps forward 
take the current list of Robots, and an order to move one of them,
check if the target square is free, if not, try to move the robot in the target square as well, and then move
-}
moveBot n i robos 
    | abs(n) ==  1 = let (Robot pos d) = robos !! i
                         move (x,y) -- translate robot movement order into a movement in a particular direction on the board
                           | d == 0 = (x,y+n)
                           | d == 1 = (x+n,y)
                           | d == 2 = (x,y-n)
                           | d == 3 = (x-n,y)
                     in tryMove robos i move
    | n > 1 = moveBot (n-1) i (moveBot 1 i robos)
    | n <(-1) = moveBot (n+1) i (moveBot (-1) i robos) -- probably don't need this case
                       
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
                                          
rotateBot n i robos = let (Robot pos direction) = robos !! i
                          robo' = (Robot pos (rotate n direction))
                      in (take i robos) ++ robo' : (drop (1+i) robos)
                   
programSteps = [("move 1", moveBot 1)
               ,("move 2", moveBot 2)
               ,("move 3", moveBot 3)
               ,("turn left", rotateBot (-1))
               ,("turn right", rotateBot 1)
               ,("back up", moveBot (-1))]
            
makeCoord (x,y) = pt (x*40) (400 -(y*40))
                                 
setProgram vProgram choiceLists 
  = let getProgram i = mapM 
                       (\c -> (do sel <-get c selection
                                  return ((snd (programSteps !! sel)) i)))
    in do ps <- zipWithM getProgram [0..(length robonames -1)] choiceLists
          varSet vProgram (sortmoves ps)
          
sortmoves:: [[a]] -> [a]
sortmoves ([]:ms) = []
sortmoves movelists = let now = map head movelists
                          future = map tail movelists
                      in (now) ++ (sortmoves future)
                                 
game = do f <- frameFixed [text := "Boro Larry"]
          vrobos <- varCreate $ map (\p -> (Robot p 0)) startpos
          vProgram <- varCreate []
          sprites <- loadBitmaps
          tile <- bitmapCreateFromFile( "./tile.png")
          p <- panel f [on paint := drawGame vrobos sprites tile]
          programSlots <- replicateM (length robonames) 
                          $ replicateM 5 (choice f  
                                          [items := map fst programSteps])
          movetimer <- timer f [interval := 500, enabled := False]
          goButton <- button f [text:= "Go!", on command:= 
                                              runIfOk 
                                              programSlots 
                                              (setProgram vProgram programSlots
                                               >> runProgram vProgram vrobos f p movetimer)]
          set f [ layout :=  
                  (column 5 [floatCentre $ minsize (sz maxX maxY) $ (widget p)
                            ,floatCentre $ programChooser programSlots
                            ,alignLeft $ widget goButton])]
          set p [ on (charKey 'f') := (varUpdate vrobos (moveBot 1 0)) >> repaint p
                , on (charKey 'b') := (varUpdate vrobos (moveBot (-1) 0)) >> repaint p
                , on (charKey 't') := (varUpdate vrobos (rotateBot 1 0)) >> repaint p
                , on (charKey 'r') := (varUpdate vrobos (rotateBot (-1) 0)) >> repaint p
                , on (charKey 'g') := (runProgram vProgram vrobos f p movetimer)]
            
-- programChooser: layout all the movement selection widgets in a
-- (nRobots x nSteps) table.
programChooser :: [[Choice a]] -> Layout
programChooser choices = column 3
                         (map 
                          ((row 3).(map widget))
                          $ choices )
                         
runIfOk programSlots action
  = do ok <- checkProgram programSlots
       if ok 
         then action
         else return ()
-- checkProgram: pass the list of movement choices and see if choices
-- have been made for all of them (check if any are still at -1)
checkProgram programSlots
  = let getSelections = mapM (\c -> get c selection)
                        (concat programSlots)
    in do sels <- getSelections
          return $ not $ elem (-1) sels

-- From the names in 'robonames' generate a list of lists with their sprites:
-- [ [spunky0.png,spunky1.png,...,spunky3.png], [bimbot0.png, ....]]
loadBitmaps 
  = sequence ( do roboname <- robonames
                  return ( mapM (\x -> bitmapCreateFromFile ( "./img/" ++ roboname ++ (show x) ++ ".png")) [0..3]))
    
drawBot' dc robo sprites 
  = let screencoords = (makeCoord  (rcoords robo))
    in drawBitmap dc (sprites !! (direction robo)) screencoords True []

drawGame vrobos sprites tile dc viewArea
  = do set dc [brushColor := black, brushKind := BrushSolid]
       mapM_ (\p -> drawBitmap dc tile p False []) coords
       robos <- varGet vrobos
       zipWithM_ (drawBot' dc) robos sprites 

runProgram vProgram vrobos f p t = set t [ on command := stepProgram t >> repaint p
                                         , enabled :~ not]
  where stepProgram t = do program <- varGet vProgram
                           if (length program >= 1)
                             then do varUpdate vrobos (program !! 0)
                                     varSet vProgram (tail program)
                             else set t [enabled :~ not]
