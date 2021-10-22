getSize :: Int
getSize = 9
-- =================================== --
-- IO & managment for MineSweeper
-- ================================== --

--TODO - DONE AFTER ALL OTHERS (dependent on others, so subject to most change)
--Main() - calls initial minesweeper builder and looper
    -- Takes input
    -- If input is valid, 
        -- build a random spread of bombs,
        -- use it to build a bomb array
        -- Use that to build the final array
        -- send it to the IOLoop
    --else
        --try again

{-
main :: IO ()
main = do
  IOLoop (proxLoop (createArray getSize) 0 0 getSize) "Give a position (eg. x y): "
-}

--TODO - DONE AFTER OTHERS (dependent on others other than Main, so subject to large change)
--IOLoop() - (prints, waits, parses user input,)
    -- Prints the passed array
    -- Print update message ("game Over", "Input a postion ", "Invalid Positon, tyr again ", etc)
    -- Takes user input
    -- Checks if its valid
        -- if so, update array
        -- Otherwise, try again
    -- Check if updated array is valid
        -- If so, loop
        -- Otherwise, end condition

{-
IOLoop:: [[[Int]]] -> String -> IO ()
IOLoop array message = do
    putStrLn (printField3D array 0 0 getSize)
    putStrLn message

    position <- getLine

    if (checkInputValidity words position) -- if valid input
    then
        if(isBomb x y)
            then putStrLn (concat [(printField3DComplete array 0 0), ['\n'], "Game Over"])
        else IOLoop (updateMap array x y) "Give a position (eg. x y): "
    --should also check if any end-game characteristics have been met
    else
        getInputTest array "Invalid Input. Give a position (eg. x y): "--invalid input

-}

--TODO
--validInput() - take a string, parse it to an [x,y] coordinate pair, along with the operation specifier (flag, dig, unflag)
    -- take a string, parse it to an [x,y] coordinate pair
    -- Verify
        -- Is the position actually there? If so,
            -- Is it a bomb?
            -- Has it already been dug?
    -- Return a value that the calling function can use for further procedure
validInput :: String -> [Int]
validInput input = [0,0,0]


-- ============================================




-- =================================== --
--Initial BOMB FIELD REQUIREMENTS
-- ================================== --

--TODO (Hint, use proxLoop as a guide for array building recursively)
-- initializeBombArray() -- Use an array of bomb positions to construct a 2d array
    -- Use a passed array of bomb positions
        -- Loop - create initial array
            -- Check if the current position is a bomb
                -- If so, set position to 1
                -- Otherwise, 0
            -- nextPosition
    -- Return the created array
initializeBombArray :: [Int] -> [Int] -> [Int]
initializeBombArray bombArray currentField = [0,0]


--TODO (Note that this will likely have to deal with IO due to generatePair)
-- getBombPositons() -- Create an array of random bomb positions
    -- Take an initial [x,y] coordinate pair (where a bomb cannot be placed)
    -- Create an array to house coordinate pairs
    -- Loop
        -- Randomly generate a pair
        -- check if it does not already exist in the array or in the initial position
            -- If so, add it and decrement a bomb count value
            -- Otherwise, try again
    -- Return the sorted array (by y then x)
getBombPositions :: Int -> Int -> Int -> [Int] -> [Int]
getBombPositions x y count bombArray = [0,0]


--TODO (Note that this will have to deal with IO sideeffect)
--generatePair -- gives back a random position [x,y]
    --[take time mod by size, take new time and mod by size] 
generatePair :: [Int]
generatePair = [0,0];

-- ========================================



-- =================================== --
-- 3D Field builders
-- ================================== --

--  MineField defined - 3d array of row, col, params -> [[[isVisible, ProximityCount, isBomb], [isVisible, ProximityCount, isBomb], ...]] - where isVisible has 3 options, yes, no, flag

--takes the initial 2d bomb array, and the size (square)
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

-- ============================================



-- =================================== --
-- Array IO Helper functions
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
                then  (concat [show (getPrintableCharacterEnd ((array!!y)!!x)), ['\n'], (printField3DComplete array 0 (y+1) size)])
        else
        if (y==size)
            then (show (getPrintableCharacterEnd ((array!!y)!!x)))
        else concat [show (getPrintableCharacterEnd ((array!!y)!!x)), (printField3DComplete array (x+1) y size)]

--used during game to print currently known board
getPrintableCharacter :: [Int] -> String
getPrintableCharacter array
  | array!!2 == 0 = "?" -- lets say 0 is specifier for unknown, and 2 is index for visibilityDesignator
  | array!!2 == 1 = "X" -- lets say 1 is specifier for flag
  | otherwise = show (array!!1) -- where m is the index for the proximity count

--used at the end of the game to print all areas
getPrintableCharacterEnd :: [Int] -> String
getPrintableCharacterEnd array
  | array!!0 == 0 = show (array!!1) -- if its not a bomb, then print proximity
  | otherwise = "B" -- if it is a bomb, print B


-- checkIsBomb(array, x, y) - to quickly check if space is a bomb
isBomb3D :: [[[Int]]] -> Int -> Int -> Bool
isBomb3D array x y = 
    1==(((array!!y)!!x)!!0)

{- --unlikely to be used
isBomb2D :: [[Int]] -> Int -> Int -> Bool
isBomb2D array x y = 
    1==((array!!y)!!x)
-}

--TODO
-- fieldUpdate(array, x,y) - take array as reference return new array with visibilityDesignator (index 2), now set to value
--the current optional implementation specifies location, and uses 2 other values (currentX and currentY for recursive indexing)
fieldUpdate :: [[[Int]]] -> Int -> Int -> Int -> Int -> Int -> [[[Int]]]
fieldUpdate array x y currentX currentY value = [[[0]]]
    --if y == countY 
        --then reassemble (replace array!!y x)
    --else
        --fieldUpdate array x y currentX+1 currentY+1
