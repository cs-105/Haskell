--Main will call this function

module Solver (solve) where
import Data.List (sort)

solve :: IO ()
solve = putStrLn "hi"
--solve =  (isValidFieldInitial (getVisible field) bombPositions gamePreset)

--inputs field
--deal with visibility to be fair

--Return nothing, instead implement different way of updating field within main.hs

--Two possible calls:
--iterative (solve by turn)
--loop (solve all)


isValidFieldInitial:: [[Int]] -> [[Int]] -> [Int] -> Bool
isValidFieldInitial fieldProximities bombPositions gamePreset =
  (length bombPositions /=  gamePreset!!2) && isValidFieldLoop fieldProximities bombPositions  --gamePrest!!2 = bombcount

isValidFieldLoop:: [[Int]] -> [[Int]] -> Bool
isValidFieldLoop fieldProximities [] = isAllZeros fieldProximities --when all the bombs have been decremented
isValidFieldLoop fieldProximities (bomb:otherBombs) = do
    let tempField = decrementNeighbors fieldProximities (bomb!!0) (bomb!!1)
    (tempField /= []) && isValidFieldLoop tempField otherBombs

-- ==========================
-- decrement
-- =================
decrementNeighbors :: [[Int]] -> Int -> Int -> [[Int]]
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

--takes the current game array, and an array of all the xvalues to be changed in the given row, returns an empty array if the field becomes invalid (prox's under 0)
updateDecrement :: [[Int]] -> [Int] -> Int -> [[Int]]
updateDecrement array xPositions y =
    updateDecrementCol array xPositions y 0

--gets to the correct row
updateDecrementCol :: [[Int]] -> [Int] -> Int -> Int -> [[Int]]
updateDecrementCol (currentRow:restOfRows) xPositions y currentY =
    if y==currentY
        then updateDecrementRow currentRow xPositions 0 : restOfRows
    else
        currentRow : updateDecrementCol restOfRows xPositions y (currentY+1)

--gets to the correct element in the row, and updates, and continues down the row if there are multiple to be changed.
  --return an empty array if it ever fails (means that we need if statments to pass it all the way back up the recursion stack)
updateDecrementRow :: [Int] -> [Int] -> Int -> [Int]
updateDecrementRow (currentProximity:restOfProxInRow) (currentTargetX:otherXs) currentX =
    if currentTargetX==currentX
        then if currentProximity > 0
            then if null otherXs
              then (currentProximity-1) : restOfProxInRow --rebuild the prox array
              else do
                let temp = updateDecrementRow restOfProxInRow otherXs (currentX+1)
                if null temp
                  then []
                else (currentProximity-1) : temp --rebuild the prox array
        else []
    else do
      let temp = updateDecrementRow restOfProxInRow (currentTargetX:otherXs) (currentX+1) --iterate down the array
      if null temp
        then []
      else
        currentProximity : temp --rebuild the array

-- ======================================
-- Check all known prox's are 0
-- ===================================
--takes the current game array, and an array of all the xvalues to be changed in the given row, returns an empty array if the field becomes invalid (prox's under 0)
isAllZeros :: [[Int]] -> Bool
isAllZeros [] = 1/=1
isAllZeros (row:restOfArray) =
  if null restOfArray
    then isZerosRow row
  else
    isZerosRow row && isAllZeros restOfArray


isZerosRow :: [Int] -> Bool
isZerosRow [] = 1/=1
isZerosRow (element:restOfRow) =
  if null restOfRow
    then isZero element
  else
    isZero element && isZerosRow restOfRow

isZero :: Int -> Bool
isZero positionValues =
  positionValues == 0 ||  positionValues < (-9) -- the decrement should never get to -9, so if it does, we know to discard it from the pool (its unknown or flagged)


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
getVisibleRow (element:restOfRow) =
  if null restOfRow
    then [getVisibleValue element]
  else
    getVisibleValue element : getVisibleRow restOfRow

getVisibleValue :: [Int] -> Int
getVisibleValue positionValues =
  if (positionValues!!2) == 2
      then positionValues!!1 --if dug (visible), return prox
      else (-10)-(positionValues!!2)  --if its unknown, it'll be -10, flag will be -11
-- ============================================
