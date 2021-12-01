--Main will call this function

module Solver (solverMain, getVisible, isValidFieldInitial) where
import Data.List (sort, findIndex)

solve :: IO ()
solve = putStrLn "hi"
--solve =  (isValidFieldInitial (getVisible field) bombPositions gamePreset)

--inputs field
--deal with visibility to be fair

--Return nothing, instead implement different way of updating field within main.hs

--Two possible calls:
--iterative (solve by turn)
--loop (solve all)

-- ==========================
-- main
-- =================

solverMain :: [[[Int]]] -> Int -> [Int]
solverMain field bombCount = do
  let visibleArray = (getVisible field)
  let flagsAndUnkownsBorderingKnowns = solveLoopInitial visibleArray
  solveFinal (flagsAndUnkownsBorderingKnowns!!1) (flagsAndUnkownsBorderingKnowns!!0) bombCount visibleArray --index 1 is unknowns, 0 is the flags


--solve takes numOfBombs (gamePreset!!2), arrayOfFlagPositions, arrayOfUnknownsNeighboringKnowns 




-- ==========================



-- ==========================
-- flag and unknownNeigboringKnwon accumulator loops -tested and fixed
-- =================

solveLoopInitial :: [[Int]] -> [[[Int]]]
solveLoopInitial array = solveLoop array 0 0 [] []

solveLoop :: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[[Int]]]
solveLoop array x y arrayOfFlagPositions arrayOfUnknownsNeighboringKnowns
  | x == (length (array!!0)) = solveLoop array 0 (y+1) arrayOfFlagPositions arrayOfUnknownsNeighboringKnowns
  | y == (length array) = [arrayOfFlagPositions, arrayOfUnknownsNeighboringKnowns]
  | otherwise = solveCurrent array x y arrayOfFlagPositions arrayOfUnknownsNeighboringKnowns

solveCurrent :: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]] -> [[[Int]]]
solveCurrent array x y arrayOfFlagPositions arrayOfUnknownsNeighboringKnowns
  | ((array!!y)!!x) == (-11) = solveLoop array (x+1) y ([x,y]:arrayOfFlagPositions) arrayOfUnknownsNeighboringKnowns
  | (((array!!y)!!x) == (-10) && (hasKnownNeighbors array x y)) = solveLoop array (x+1) y arrayOfFlagPositions ([x,y]:arrayOfUnknownsNeighboringKnowns)
  | otherwise = solveLoop array (x+1) y arrayOfFlagPositions arrayOfUnknownsNeighboringKnowns

hasKnownNeighbors :: [[Int]] -> Int -> Int -> Bool
hasKnownNeighbors array x y =
  hasKnownNeighborsT array x y

hasKnownNeighborsT :: [[Int]] -> Int -> Int -> Bool
hasKnownNeighborsT array x y
  | y > 0 && x > 0 && x < ((length (array!!0))-1) = ((array!!(y-1))!!(x-1)) >= 0 || ((array!!(y-1))!!(x)) >= 0 || ((array!!(y-1))!!(x+1)) >= 0 || hasKnownNeighborsM array x y --Any on top
  | y > 0 && x > 0                                = ((array!!(y-1))!!(x-1)) >= 0 || ((array!!(y-1))!!(x)) >= 0 || hasKnownNeighborsM array x y --Not TR
  | y > 0 && x < ((length (array!!0))-1)          = ((array!!(y-1))!!(x)) >= 0 || ((array!!(y-1))!!(x+1)) >= 0 || hasKnownNeighborsM array x y --Not TL
  | otherwise = hasKnownNeighborsM array x y

hasKnownNeighborsM :: [[Int]] -> Int -> Int -> Bool
hasKnownNeighborsM array x y
  | x > 0 && x < ((length (array!!0))-1)          = ((array!!(y))!!(x-1)) >= 0 || ((array!!(y))!!(x)) >= 0 || ((array!!(y))!!(x+1)) >= 0 || hasKnownNeighborsB array x y --Either L or R
  | x > 0                                         = ((array!!(y))!!(x-1)) >= 0 || ((array!!(y))!!(x)) >= 0 || hasKnownNeighborsB array x y --Not R
  | x < ((length (array!!0))-1)                   = ((array!!(y))!!(x)) >= 0 || ((array!!(y))!!(x+1)) >= 0 || hasKnownNeighborsB array x y--Not L
  | otherwise = hasKnownNeighborsB array x y

hasKnownNeighborsB :: [[Int]] -> Int -> Int -> Bool
hasKnownNeighborsB array x y
  | y < ((length array)-1) && x > 0 && x < ((length (array!!0))-1) = ((array!!(y+1))!!(x-1)) >= 0 || ((array!!(y+1))!!(x)) >= 0 || ((array!!(y+1))!!(x+1)) >= 0 --Any on bottom
  | y < ((length array)-1) && x > 0                                = ((array!!(y+1))!!(x-1)) >= 0 || ((array!!(y+1))!!(x)) >= 0 --Not BR
  | y < ((length array)-1) && x < ((length (array!!0))-1)           = ((array!!(y+1))!!(x)) >= 0 || ((array!!(y+1))!!(x+1)) >= 0 --Not BL
  | otherwise = 1==0

-- ==========================



isValidFieldInitial:: [[Int]] -> [[Int]] -> Int -> Int
isValidFieldInitial fieldProximities bombPositions bombCount =
  if ((length bombPositions <=  bombCount) && (isValidFieldLoop fieldProximities bombPositions))  then 1 else 0 --gamePrest!!2 = bombcount

isValidFieldLoop:: [[Int]] -> [[Int]] -> Bool
isValidFieldLoop fieldProximities [] = (1==1) --when all the bombs have been decremented
isValidFieldLoop fieldProximities (bomb:otherBombs) = do
    let tempField = decrementNeighbors fieldProximities (bomb!!0) (bomb!!1)
    (tempField /= []) && isValidFieldLoop tempField otherBombs

-- ==========================
-- decrement
-- =================
decrementNeighbors :: [[Int]] -> Int -> Int -> [[Int]]
decrementNeighbors field x y = updateDecrement (updateDecrement (updateDecrement field [(x-1),(x+1)] y)  [(x-1),x,(x+1)] (y+1)) [(x-1),x,(x+1)] (y-1)  
{-
  | x == (length (field!!0))-1 && y==0 = updateDecrement (updateDecrement field [(x-1)] y)  [(x-1),x] (y-1)                         --top right - scan L,LB,B
  | x == 0 && y==0 = updateDecrement (updateDecrement field [(x+1)] y)  [x,(x+1)] (y-1)                                             --top left - scan R,RB,B
  | y == 0 = updateDecrement (updateDecrement field [(x-1),(x+1)] y)  [(x-1),x,(x+1)] (y-1)                                         --top middle - scan L,LB,B,RB,R
  | x == (length (field!!0))-1 && y==(length field)-1 = updateDecrement (updateDecrement field [(x-1)] y)  [(x-1),x] (y+1)                   --bottom right - scan T,TL,L
  | x == 0 && y==(length field)-1 = updateDecrement (updateDecrement field [(x+1)] y)  [x,(x+1)] (y+1)                              --bottom left - scan T,TR,R
  | y==(length field)-1 = updateDecrement (updateDecrement field [(x-1),(x+1)] y)  [(x-1),x,(x+1)] (y+1)                            --bottom middle - scan L,TL,T,TR,R
  | x == 0 = updateDecrement (updateDecrement (updateDecrement field [(x+1)] y)  [x,(x+1)] (y+1)) [x,(x+1)] (y-1)                   --left - scan T,TR,R,B,BR
  | x == (length (field!!0))-1 = updateDecrement (updateDecrement (updateDecrement field [(x-1)] y)  [(x-1),x] (y+1)) [(x-1),x] (y-1)   --right - scan T,TL,L,BL,B
  | otherwise = updateDecrement (updateDecrement (updateDecrement field [(x-1),(x+1)] y)  [(x-1),x,(x+1)] (y+1)) [(x-1),x,(x+1)] (y-1)                    --middle/center - scan T,TR,R,BR,B,BL,L,TL
-}
--

--takes the current game array, and an array of all the xvalues to be changed in the given row, returns an empty array if the field becomes invalid (prox's under 0)
updateDecrement :: [[Int]] -> [Int] -> Int -> [[Int]]
updateDecrement array xPositions y =
  if(y<0 || y>= (length array)) then array -- if y is outside the bounds of the array, return an unaltered array
  else
    updateDecrementCol array xPositions y 0 -- decrement the specific values in the given row

--gets to the correct row
updateDecrementCol :: [[Int]] -> [Int] -> Int -> Int -> [[Int]]
updateDecrementCol [] xPositions y currentY = []
updateDecrementCol (currentRow:restOfRows) xPositions y currentY =
    if y==currentY -- made it to correct yvalue, decrement
        then updateDecrementRow currentRow xPositions 0 (length currentRow) : restOfRows 
    else
        currentRow : updateDecrementCol restOfRows xPositions y (currentY+1)

--gets to the correct element in the row, and updates, and continues down the row if there are multiple to be changed.
  --return an empty array if it ever fails (means that we need if statments to pass it all the way back up the recursion stack)
updateDecrementRow :: [Int] -> [Int] -> Int -> Int -> [Int]
updateDecrementRow [] (currentTargetX:otherXs) currentX sizeX = []
updateDecrementRow (currentProximity:restOfProxInRow) [] currentX sizeX = (currentProximity:restOfProxInRow)
updateDecrementRow (currentProximity:restOfProxInRow) (currentTargetX:otherXs) currentX sizeX
  | (currentTargetX<0 || currentTargetX >= (sizeX)) = updateDecrementRow (currentProximity:restOfProxInRow) otherXs currentX sizeX --invalid x value outside bounds
  | currentTargetX==currentX = if currentProximity > 0 || currentProximity < (-9) --made it to valid target
                              then if null otherXs
                                then (currentProximity-1) : restOfProxInRow --rebuild the prox array if there are no other values to update
                                else do
                                  let temp = updateDecrementRow restOfProxInRow otherXs (currentX+1) sizeX
                                  if null temp --if invalid
                                    then []
                                  else (currentProximity-1) : temp --rebuild the prox array with the array created from the rest of the functons
                          else [] --invalid proxArray, too many bombs for the prox
  | otherwise = do --if not quite at the targetX
  let temp = updateDecrementRow restOfProxInRow (currentTargetX:otherXs) (currentX+1) sizeX --iterate down the array
  if null temp -- double check its a valid
    then []
  else
    currentProximity : temp --rebuild the array with the later calls

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
-- Create an array of values for the solver to use, tested, works
-- ================================== --
getVisible :: [[[Int]]] -> [[Int]]
getVisible [] = []
getVisible (row:restOfArray) =
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

--this function takes in the results from decisionArray and returns a useful
--object which defines teh best move.
--the input is as follows
--  Decision Array:
--      output of the function decisionArray
--  0 = index of best move
--  1 = action to be taken (1 = flag as bomb, 2 = dig as not bomb)
bestMoveHelper :: [[Int]] -> Int -> [Int]
bestMoveHelper dArr totalValid =
    --This branch finds if we can gurantee a safe space
    if (maximum ( foldr (++) ([]) (map (take 1) (dArr)) ) == totalValid)
        then [fromJust (findIndex (== totalValid) (foldr (++) ([]) (map (take 1) (dArr)))), 2]
        else if (maximum ( foldr (++) ([]) (map (drop 1) (dArr)) ) == totalValid)
            then [fromJust (findIndex (== totalValid) (foldr (++) ([]) (map (drop 1) (dArr)))), 1]
            else if (totalValid == 0)
                then [-1, -1]
                else bestMoveHelper dArr (totalValid - 1)

--pulled this from the following, thanks 
--https://stackoverflow.com/questions/4940349/how-to-get-the-value-of-a-maybe-in-haskell
fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

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
probs :: [[Int]] -> [[Int]] -> Int -> [[Int]] -> [Int]
probs targetedUnkowns acceptedBombs totalBombs visibleArray
    | (length acceptedBombs) >= totalBombs = [0]
    | length targetedUnkowns == 0 = [isValidFieldInitial visibleArray acceptedBombs totalBombs] -- TODO type mismatch, you'll have to pass it down
    | otherwise = (  probs (tail targetedUnkowns) (acceptedBombs) totalBombs  visibleArray ) ++
                  (  probs (tail targetedUnkowns) (acceptedBombs ++ [head targetedUnkowns]) totalBombs visibleArray)

isValidBoardState :: [[Int]] -> Int
isValidBoardState board = 1

--this is it! the function you've been looking for! above this are all the wonderful
--functions which make a function such as this possible. This function takes in...
--I don't fully understand what the visibleArray is, but Andrew needs it for something
solveFinal :: [[Int]] -> [[Int]] -> Int -> [[Int]] -> [Int]
solveFinal targetedUnkowns acceptedBombs totalBombs visibleArray =
    do
    let dArr = decisionArray (probs targetedUnkowns acceptedBombs totalBombs visibleArray) --[[vState,!vState]]
    let bestMoveVar = bestMoveHelper (dArr) (sum (dArr !! 0)) --[index,action] action == 0 or 2
    let numerator = if ((bestMoveVar !! 1) == 2) then (1) else (0)
    let finalOutput =
                    [ ((targetedUnkowns !! (bestMoveVar !! 0)) !! 0)      --x
                    , ((targetedUnkowns !! (bestMoveVar !! 0)) !! 1)      --y
                    , (bestMoveVar !! 1)                                  --action
                    , numerator                                           --numerator
                    , (sum (dArr !! 0))      ]                            --denominator
    finalOutput

--THIS IS ANDREWS CODE--
--isValidFieldInitial:: [[Int]] -> [[Int]] -> [Int] -> Bool
--isValidFieldInitial fieldProximities bombPositions gamePreset =
--THIS IS ANDREWS CODE--

--IF IT IS A BOMB IT GOES RIGHT
