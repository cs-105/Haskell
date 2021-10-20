
getSize :: Int
getSize = 9
-- =================================== --
-- IO & managment for MineSweeper
-- ================================== --

--Main() - calls initial minesweeper builder and looper
    --print empty array
    --looper(intialBuidl(take input))
{-
main :: IO ()
main = do
  --Maybe proxloop should initialize visible aspect as well?
  getInputLoop (proxLoop (createArray getSize) 0 0 getSize) "Give a position (eg. x y): "
-}

--Looper() - (prints, waits, and parses user input)
    --Print the passed Array
    --Parses input (test whether better to do string operations or multiple seperate inputs)
    --Sends input to mineFieldUpdate get array
    --Send array to the Looper along with the message
{-
getInputLoop:: [[[Int]]] -> String -> IO ()
getInputLoop array message = do
    putStrLn (printField3D array 0 0 getSize)
    putStrLn message

    position <- getLine

    if (checkInputValidity words position) -- if valid input
    then
        if(isBomb x y)
            then putStrLn (concat [(printField3DComplete array 0 0), ['\n'], "Game Over"])
        else getInputLoop (updateMap array x y) "Give a position (eg. x y): "
    --should also check if any end-game characteristics have been met
    else
        getInputTest array "Invalid Input. Give a position (eg. x y): "--invalid input

-}
--MineField - 3d array of row, col, params -> [[[isVisible, ProximityCount, isBomb], [isVisible, ProximityCount, isBomb], ...]] - where isVisible has 3 options, yes, no, flag
-- ============================================




-- =================================== --
--Initial MINE FIELD REQUIREMENTS
-- ================================== --

--initialBuild() - creates the array 
    --get an empty array - use hard code or loop (this may be just a simple 2d array at this point) that was used for the main call
    --return populateWithBombs(bombNum, emptyArray, initialPosition) - where bombNum is the number of bombs left to be added, emptyArray is the correct size that we want for further reference, initialPosition is where a bomb must not be

--populateWithBombs () - due to the annoyances of nested if statements with multiple lines, lines 31-35 (inner ifelse) may be better suited in their own minor function
    --if the count is still not 0
        --generate a random position (an x and a y, or go linear)
        --if the position doesnt already have a bomb in it (and is not the initial guess), update the array 
            --(increase surrounding proximities by 1, and change isBomb to true)
            --feed it back into the function until appropriate number of bombs exist
        --else populateWithBombs(bombNum, array, initialPosition)
    --else
        --return populateProximitiesXPos(array)

-- =================================== --
-- 3D Field builders
-- ================================== --

--populateProximitiesXPos () -- uses isBomb array as reference (remember to address whether the initial array is in fact still 2d, we'll be retruning a 3d)
    --move left to right per row adding isBomb to the nextProximity as it goes
    --return populateProximitiesXNeg(newArray)

--populateProximitiesXNeg () -- uses isBomb & proximities array as reference
    --move right to left per row adding isBomb to the nextProximity as it goes
    --return the call to next populatePrximties

--populateProximitiesYPos ()
--populateProximitiesYNeg () 
--populateProximitiesXPosYNeg () -- right and down
--populateProximitiesXPosYPos () -- right and up
--populateProximitiesXNegYNeg () -- left and down
--populateProximitiesXNegYPos () --left and up
    --return array


--ALTERNATIVELY loop through the existing array populating it with proximities linearly using a helper

--takes the initial 2d bomb array
--returns a 3d array filled with cols, rows, [isBomb, proximity, visibilityDesignator]
proxLoop :: [[Int]] -> Int -> [[[Int]]]
proxLoop array size =
  proxLoopCol array 0 size


--loop that builds rows returns ([[array!!y!!x, getProx array x y size] : nextElements array x+1 y size)] - returns 2d array of [isBomb, proximityCount, visibilityDesignator] pairs
proxLoopRow :: [[Int]] -> Int -> Int -> Int -> [[Int]]
proxLoopRow array x y size
  | x == size = [[((array!!y)!!x),(getProx array x y size), 0]] --base case, when done with a row, return the full row
  | otherwise = [((array!!y)!!x),(getProx array x y size), 0] : (proxLoopRow array (x+1) y size)

--loop that compiles rows [buildRow array x y size : compileRows array x y+1 size] - returns 3d array
proxLoopCol :: [[Int]] -> Int -> Int -> [[[Int]]]
proxLoopCol array y size
  | y == size = [(proxLoopRow array 0 y size)] --base case, when done with a row, return the full row
  | otherwise = (proxLoopRow array 0 y size) : (proxLoopCol array (y+1) size)


--params = [[Isbomb]], row, col, sizeOfGrid
--returns the number of bombs in proximity
getProx :: [[Int]] -> Int -> Int -> Int -> Int
getProx array x y size
  | (y == 0 || y == size) = if (y==0) --if top
                           then do
                               if (x == 0 || x == size) --if its on either of the extremes x
                               then do
                                   if(x==0) -- if top left
                                       then ((array!!y)!!(x+1))+((array!!(y+1))!!(x+1))+((array!!(y+1))!!(x))
                                   else --if top right
                                       ((array!!y)!!(x-1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))
                               else --if its in the top middle
                                   ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y+1))!!(x+1))
                       else
                           do
                               if (x == 0 || x == size) --if its on either of the extremes x
                               then do
                                   if(x==0) -- if bottom left
                                       then ((array!!y)!!(x+1))+((array!!(y-1))!!(x+1))+((array!!(y-1))!!(x))
                                   else --if bottom right
                                       ((array!!y)!!(x-1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))
                               else --if its in the bottom middle
                                   ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))+((array!!(y-1))!!(x+1))
  | (x == 0 || x == size) = do
        if(x==0) -- if left
            then ((array!!y)!!(x+1))+((array!!(y+1))!!(x+1))+((array!!(y+1))!!(x))+((array!!(y-1))!!(x+1))+((array!!(y-1))!!(x))
        else --if right
            ((array!!y)!!(x-1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))
  | otherwise = --if its in the middle
        ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y+1))!!(x+1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))+((array!!(y-1))!!(x+1))

--Old Code, do not delete until after bound version is tested
{- --params = [[Isbomb]], row, col, sizeOfGrid
--returns the number of bombs in proximity
getProx :: [[Int]] -> Int -> Int -> Int -> Int
getProx array x y size = 
    if (y == 0 || y == size)  -- if on (top || bottom)
        then if (y==0) --if top
            then do
                if (x == 0 || x == size) --if its on either of the extremes x
                then do
                    if(x==0) -- if top left
                        then ((array!!y)!!(x+1))+((array!!(y+1))!!(x+1))+((array!!(y+1))!!(x))
                    else --if top right
                        ((array!!y)!!(x-1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))
                else --if its in the top middle
                    ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y+1))!!(x+1))
        else
            do
                if (x == 0 || x == size) --if its on either of the extremes x
                then do
                    if(x==0) -- if bottom left
                        then ((array!!y)!!(x+1))+((array!!(y-1))!!(x+1))+((array!!(y-1))!!(x))
                    else --if bottom right
                        ((array!!y)!!(x-1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))
                else --if its in the bottom middle
                    ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))+((array!!(y-1))!!(x+1))
    else
        if (x == 0 || x == size) --if its on either of the extremes x
            then do
                if(x==0) -- if left
                    then ((array!!y)!!(x+1))+((array!!(y+1))!!(x+1))+((array!!(y+1))!!(x))+((array!!(y-1))!!(x+1))+((array!!(y-1))!!(x))
                else --if right
                    ((array!!y)!!(x-1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))
        else --if its in the middle
                ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y+1))!!(x+1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))+((array!!(y-1))!!(x+1))
 -}

-- if (y == 0 || y == size)  -- if on (top || bottom)
--     if (y == 0 || y == size) --if (left || right)
-- 		if(left)
-- 		else (right)
-- 	else (in middle)
-- else (its in the middle)
-- 	if (left || right)
-- 		if(left)
-- 		else(right)
-- 	else (its in the middle)
-- ============================================


-- =================================== --
-- Array Helper functions
-- ================================== --

-- allows creates a singular string from the completed 3d array
printField3D :: [[[Int]]] -> Int -> Int -> Int -> String
printField3D  array x y size =
        if (x == size && y /= size)
                then  (concat [getPrintableCharacter ((array!!y)!!x), ['\n'], (printField3D array 0 (y+1) size)])
        else
        if (y==size) then (getPrintableCharacter ((array!!y)!!x))
                else concat [(getPrintableCharacter ((array!!y)!!x)), (printField3D array (x+1) y size)]

--call when the game is over
printField3DComplete :: [[[Int]]] -> Int -> Int -> Int -> String
printField3DComplete  array x y size =
        if (x == size && y /= size)
                then  (concat [show (getPrintableCharacter ((array!!y)!!x)), ['\n'], (printField3DComplete array 0 (y+1) size)])
        else
        if (y==size)
            then (show (getPrintableCharacter ((array!!y)!!x)))
        else concat [show (getPrintableCharacter ((array!!y)!!x)), (printField3DComplete array (x+1) y size)]

getPrintableCharacter :: [Int] -> String
getPrintableCharacter array
  | array!!2 == 0 = "?" -- lets say 0 is specifier for unknown, and 2 is index for visibilityDesignator
  | array!!2 == 1 = "X" -- lets say 1 is specifier for flag
  | otherwise = show (array!!1) -- where m is the index for the proximity count


-- checkIsBomb(array, x, y) - to quickly check if space is a bomb

-- revealLocation(array, x,y) - take array as reference return new array with isVisible true as all spaces

-- revealAll(array) - may be used if player hits a bomb - take array as reference and change all elements to visible