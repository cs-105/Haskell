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
isValidFieldLoop fieldProximities (bomb:otherBombs) = do
    let tempField = decrementNeighbors fieldProximities (bomb!!0) (bomb!!1)
    (tempField /= []) && isValidFieldLoop tempField otherBombs
isValidFieldLoop fieldProximities [] = isAllZeros fieldProximities --when all the bombs have been decremented

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
updateDecrementCol (first:rest) xPositions y currentY =
    if y==currentY
        then updateDecrementRow first xPositions 0 : rest
    else
        first : updateDecrementCol rest xPositions y (currentY+1)

--gets to the correct element in the row, and updates
updateDecrementRow :: [Int] -> [Int] -> Int -> [Int]
updateDecrementRow (first:rest) (x:otherXs) currentX =
    if x==currentX
        then if first > 0
            then if null otherXs
              then (first-1) : rest
              else do
                let temp = updateDecrementRow rest otherXs (currentX+1)
                if null temp
                  then []
                else (first-1) : temp
        else []
    else do
      let temp = updateDecrementRow rest (x:otherXs) (currentX+1)
      if null temp
        then []
      else
        first : temp

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


--this function gets the first n items from an array.
--this is a helper function for ease of reading/writing.
firstNItems :: Int -> [a] -> [a]
firstNItems _ []     = []
firstNItems 0 _      = []
firstNItems n (x:xs) = [x] ++ (firstNItems (n-1) xs)

--this function will take in the list of length 2^n from probs
--then it grabs the swathes of the array that correspond with
--a specific "bomb" or level of the tree (isBomb).
bombFromProbs :: Int -> [Int] -> [Int]
bombFromProbs n result = 
    if(n == 0)
        then firstNItems ((length result) `div` 2) result
        else (bombFromProbs (n-1) (firstNItems ((length result) `div` 2) result)) ++ 
             (bombFromProbs (n-1) (reverse (firstNItems ((length result) `div` 2) (reverse result))))

--this function will fail to work properly if not passed a power of two.
--fortunately, we're the ones writing it and know that it will not.
intLogTwo :: Int -> Int
intLogTwo x =
    if (odd x)
        then 0 
        else floor (logBase 2.0 (fromIntegral x))

--this function takes in the results from probs and returns a decision array.
--the input will be of length 2^n representing the leaves of the valid states tree.
--the output will be of length n with each index representing the mine in question 
--  and an interior array of length 2, where the first number is the number of states
--  where n is not a bomb, and the second is the number of states where it is a bomb.
--
--the input will look something like [1,1,1,1,0,0,0,0] for a 3 mine question.
--the output will look something like [[4,0],[2,2],[2,2]] for a 3 mine question.
--i is a looping number that makes the recursion easier. It should be defined as
--(intLogTwo (length inArr)) when calling the method.
decisionArrayHelper :: Int -> [Int] -> [[Int]]
decisionArrayHelper _ []    = []
decisionArrayHelper 0 _     = []
decisionArrayHelper n inArr = (decisionArrayHelper (n-1) inArr) ++ 
                        [[ 
                        ( sum (bombFromProbs (n-1) inArr)), 
                        ( sum (bombFromProbs (n-1) (reverse inArr))) 
                        ]]

decisionArray :: [Int] -> [[Int]]
decisionArray inArr = decisionArrayHelper (intLogTwo (length inArr)) inArr

--this function, given all the unknowns and knowns, will generate a series of valid
--board states. The isValidBoardState function (or something similar) was written by
--Andrew, and as such will simply have some stand-in function for testing.
--params are as follows
--  targetedUnkowns:
--      targetUnknowns are the spaces the solver will be checking. These are the places
--      that bombs could potentially be.
--  acceptedBombs:
--      acceptedBombs are the spaces that the solver is assuming to be a bomb for a
--      specific recursive branch. These will include bombs already flagged/known as
--      well as bombs assigned to it from previous calls.
--  totalBombs:
--      totalBombs is the number of total bombs on the field; this is fairly straightforward,
--      and will be used to short-circuit out some intensive/redundant computation.
probs :: [[Int]] -> [[Int]] -> Int -> [Int]
probs targetedUnkowns acceptedBombs totalBombs
    | (length acceptedBombs) >= totalBombs = [0]
    | length targetedUnkowns == 0 = [isValidBoardState acceptedBombs]
    | otherwise = (  probs (tail targetedUnkowns) (acceptedBombs) totalBombs  ) ++ 
                  (  probs (tail targetedUnkowns) (acceptedBombs ++ [head targetedUnkowns]) totalBombs  )

isValidBoardState :: [[Int]] -> Int
isValidBoardState board = 1

--THIS IS ANDREWS CODE--
--isValidFieldInitial:: [[Int]] -> [[Int]] -> [Int] -> Bool
--isValidFieldInitial fieldProximities bombPositions gamePreset =
--THIS IS ANDREWS CODE--


--IF IT IS A BOMB IT GOES RIGHT