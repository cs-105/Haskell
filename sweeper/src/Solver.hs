--Main will call this function

module Solver (solve) where
import Data.List (sort)

solve :: IO ()
solve = putStrLn "hi"

--inputs field
--deal with visibility to be fair

--Return nothing, instead implement different way of updating field within main.hs

--Two possible calls:
--iterative (solve by turn)
--loop (solve all)

isValidFieldInitial:: [[[Int]]] -> [[Int]] -> [Int] -> Bool
isValidFieldInitial fieldProximities bombPositions gamePreset =
  (length bombPositions /=  gamePreset!!2) && isValidFieldLoop fieldProximities bombPositions  --gamePrest!!2 = bombcount

isValidFieldLoop:: [[[Int]]] -> [[Int]] -> Bool
isValidFieldLoop fieldProximities (bomb:otherBombs) = do
    let tempField = decrementNeighbors fieldProximities (bomb!!0) (bomb!!1)
    (tempField /= []) && isValidFieldLoop tempField otherBombs

decrementNeighbors :: [[[Int]]] -> Int -> Int -> [[[Int]]]
decrementNeighbors field x y
  | x == (length (field!!0))-1 && y==0 = updateDecrement (updateDecrement field [(x-1)] y)  [(x-1),x] (y-1)                         --top right - scan L,LB,B
  | x == 0 && y==0 = updateDecrement (updateDecrement field [(x+1)] y)  [x,(x+1)] (y-1)                                             --top left - scan R,RB,B
  | y == 0 = updateDecrement (updateDecrement field [(x-1),(x+1)] y)  [(x-1),x,(x+1)] (y-1)                                         --top middle - scan L,LB,B,RB,R
  | x == (length (field!!0))-1 && y==(length field)-1 = updateDecrement (updateDecrement field [(x-1)] y)  [(x-1),x] (y+1)                   --bottom right - scan T,TL,L
  | x == 0 && y==(length field)-1 = updateDecrement (updateDecrement field [(x+1)] y)  [x,(x+1)] (y+1)                              --bottom left - scan T,TR,R
  | y==(length field)-1 = updateDecrement (updateDecrement field [(x-1),(x+1)] y)  [(x-1),x,(x+1)] (y+1)                            --bottom middle - scan L,TL,T,TR,R
  | x == 0 = updateDecrement (updateDecrement (updateDecrement field [(x+1)] y)  [x,(x+1)] (y+1)) [x,(x+1)] (y-1)                   --left - scan T,TR,R,B,BR
  | x == (length (field!!0))-1 = updateDecrement (updateDecrement (updateDecrement field [(x-1)] y)  [(x-1),x] (y+1)) [(x-1),x] (y-1)   --right - scan T,TL,L,BL,B
  | otherwise = updateDecrement (updateDecrement (updateDecrement field [(x-1),(x+1)] y)  [(x-1),x,(x+1)] (y+1)) [(x-1),x,(x+1)] (y-1)                    --middle/center - scan T,TR,R,BR,B,BL,L,TL

--Todo
  --loop through entire known prox array and double check its all zeros(otherwies its not valid)
  --isolate the prox array from the rest

--takes the current game array, and an array of all the xvalues to be changed in the given row, returns an empty array if the field becomes invalid (prox's under 0)
updateDecrement :: [[[Int]]] -> [Int] -> Int -> [[[Int]]]
updateDecrement array xPositions y =
    updateDecrementCol array xPositions y 0

--gets to the correct row
updateDecrementCol :: [[[Int]]] -> [Int] -> Int -> Int -> [[[Int]]]
updateDecrementCol (first:rest) xPositions y currentY =
    if y==currentY
        then updateDecrementRow first xPositions 0 : rest
    else
        first : updateDecrementCol rest xPositions y (currentY+1)

--gets to the correct element in the row, and updates
updateDecrementRow :: [[Int]] -> [Int] -> Int -> [[Int]]
updateDecrementRow (first:rest) (x:otherXs) currentX =
    if x==currentX
        then if (first!!2) > 0
            then if null otherXs
              then [(first!!0), (first!!1), ((first!!2)-1)] : rest
              else do
                let temp = updateDecrementRow rest otherXs (currentX+1)
                if null temp
                  then []
                else [(first!!0), (first!!1), ((first!!2)-1)] : temp
        else []
    else do
      let temp = updateDecrementRow rest (x:otherXs) (currentX+1)
      if null temp
        then []
      else
        first : temp



-- =================================== --
-- Create an array of values for the solver to use
-- ================================== --
getVisible :: [[[Int]]] -> [[Int]]
getVisible [] = []
takeProx (row:restOfArray) =
  if null restOfArray
    then [getVisibleRow row]
  else
    getVisibleRow row : getVisible restOfArray


getVisibleRow :: [[Int]] -> [Int]
getVisibleRow [] = []
takeProxRow (element:restOfRow) =
  if null restOfRow
    then [getVisibleValue element]
  else
    getVisibleValue element : getVisibleRow restOfRow

getVisibleValue :: [Int] -> Int 
getVisibleValue positionValues = 
  if (positionValues!!2) == 2
      then positionValues!!1 --if dug (visible), return prox
      else (-1)-(positionValues!!2)  --if its unknown, it'll be -1, flag will be -2
-- ============================================
