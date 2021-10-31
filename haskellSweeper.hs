import Data.List (findIndices)
-- =================================== --
-- CONST Testing
-- ================================== --
getSize :: Int
getSize = 9

-- ================================== --



-- =================================== --
-- IO & managment for MineSweeper
-- ================================== --

--Main() - calls initial minesweeper builder and looper
main :: IO ()
main = do
    ioLoopInitial "Give a position (eg. x y): "

--IOLoop() - (prints, waits, parses user input,)
    --continually waits for correct user input, once it exists, moves on to ioLoop
ioLoopInitial :: String -> IO ()
ioLoopInitial message = do
    putStrLn (printField2D 0 0 getSize)
    putStrLn message

    position <- getLine

    let input = parseInput position
    --create initial array
    if ((length input == 3) && (input!!0)<getSize && (input!!1) < getSize) -- if valid input
    then do
        --let bombPositions = getBombPositions (input!!0) (input!!1) 10 []
        let bombPositions = example9x9BombPositions
        ioLoop (fieldUpdate (proxLoop (initializeBombArray bombPositions getSize) getSize) (input!!0) (input!!1) (input!!2)) "Give a position (eg. x y): "

    else
        ioLoopInitial "Invalid Input. Give a position (eg. x y): "--invalid input



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
ioLoop:: [[[Int]]] -> String -> IO ()
ioLoop array message = do
    --putStrLn (show (length ((array!!0)!!0)))
    putStrLn (printField3D array 0 0 getSize)
    putStrLn message

    position <- getLine

    let input = parseInput position

    --create initial array

    if validAction input array -- if valid input
    then
        if(getIsBomb (input!!0) (input!!1) array)
            then putStrLn (concat [(printField3DComplete array 0 0 getSize), ['\n'], "Game Over"])
        else ioLoop (fieldUpdate array (input!!0) (input!!1) (input!!2)) "Give a position (eg. x y): "
    --should also check if any end-game characteristics have been met
    else
        ioLoop array "Invalid Input. Give a position (eg. x y): "--invalid input




--validAction() - take [x,y,action] and the field
    -- Verify
        -- Is the position actually within bounds? 
        -- Is the region available for more actions (not already dug)?
validAction :: [Int] -> [[[Int]]] -> Bool
validAction parsedInput field =
    ((parsedInput!!0) < getSize && (parsedInput!!1) < getSize) && ((getVisibilityDesignator (parsedInput!!0) (parsedInput!!1) field)/=2)


--parseInput() - take a string, parse it to an [x,y] coordinate pair, along with the action specifier (0 - unflag, 1 - flag, 2 - dig)
    -- take a string, parse it to an [x,y] coordinate pair
    -- Verify
        -- Is the position actually there? If so,
            -- Is it a bomb?
            -- Has it already been dug?
    -- Return a value that the calling function can use for further procedure
parseInput :: String -> [Int]
parseInput input = do
    let l = (words input);
    if(length l == 3)
        then if ((length(findIndices (==(l!!0)) rowKeyArray))==1 && (length (findIndices (==(l!!2)) actionKeyArray)==1))
            then
                [(findIndices (==(l!!0)) rowKeyArray)!!0, read (l!!1), (findIndices (==(l!!2)) actionKeyArray)!!0]
            else []
    else
        if ((length(findIndices (==(l!!0)) rowKeyArray))==1)
            then [(findIndices (==(l!!0)) rowKeyArray)!!0, read (l!!1), 0]  -- if no action provided, asume unflag (least destructive)
        else
            []


--in expert mode, there are only (at max 16 rows, so we start there)
rowKeyArray :: [String]
rowKeyArray = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"]

--in expert mode, there are only (at max 16 rows, so we start there)
actionKeyArray :: [String]
actionKeyArray = ["unflag","flag","dig"]


-- ============================================




-- =================================== --
--Initial BOMB FIELD REQUIREMENTS
-- ================================== --


example9x9BombPositions :: [[Int]]
example9x9BombPositions = [[0,0],[1,8],[3,6],[5,8],[8,0],[1,5],[3,7],[5,0],[3,5],[7,6]]


-- initializeBombArray() -- Use an array of bomb positions to construct a 2d array
    -- Use a passed array of bomb positions
    -- Loop - create initial array
        -- Check if the current position is a bomb
            -- If so, set position to 1
            -- Otherwise, 0
        -- nextPosition
    -- Return the created array
initializeBombArray :: [[Int]] -> Int -> [[Int]]
initializeBombArray bombPositions size =
    bombLoopCol bombPositions 0 size

--loop that builds rows returns ([[array!!y!!x, getProx array x y size] : nextElements array x+1 y size)] - returns 2d array of [isBomb, proximityCount, visibilityDesignator] pairs
bombLoopRow :: [[Int]] -> Int -> Int -> Int -> [Int]
bombLoopRow bombPositions x y size
  | x == size-1 = if elem [x,y] bombPositions then [1] else [0]--base case, when done with a row, return the full row
  | otherwise = if elem [x,y] bombPositions then 1 : bombLoopRow bombPositions (x+1) y size else 0 : bombLoopRow bombPositions (x+1) y size

--loop that compiles rows [buildRow array x y size : compileRows array x y+1 size] - returns 3d array
bombLoopCol :: [[Int]] -> Int -> Int -> [[Int]]
bombLoopCol bombPositions y size
  | y == size-1 = [bombLoopRow bombPositions 0 y size] --base case, when done with a row, return the full row
  | otherwise = (bombLoopRow bombPositions 0 y size) : (bombLoopCol bombPositions (y+1) size)




-- getBombPositons() -- Create an array of random bomb positions
    -- Loop
        -- Randomly generate a pair
        -- check if it does not already exist in the array or in the initial position
            -- If so, add it and decrement a bomb count value
            -- Otherwise, try again
    -- Return the sorted array (by y then x)
getBombPositions :: Int -> Int -> Int -> [[Int]] -> [[Int]]
getBombPositions x y count bombArray = do
    let temp = generatePair
    if count>0
        then if elem temp bombArray || [x,y] == temp
            then getBombPositions x y count bombArray
        else
            getBombPositions x y (count-1) (temp:bombArray)
    else bombArray



--TODO (Note that this will have to deal with IO sideeffect)
--generatePair -- gives back a random position [x,y]
    --[take time mod by size, take new time and mod by size] 
generatePair :: [Int]
generatePair = [0,1];



-- ========================================



-- =================================== --
-- 3D Field builders
-- ================================== --

--  MineField defined - 3d array of row, col, params -> [[[isVisible, ProximityCount, isBomb], [isVisible, ProximityCount, isBomb], ...]] - where isVisible has 3 options, unknown, flag, dug

--takes the initial 2d bomb array, and the size (square)
--returns a 3d array filled with cols, rows, [isBomb, proximity, visibilityDesignator]
proxLoop :: [[Int]] -> Int -> [[[Int]]]
proxLoop array size =
  proxLoopCol array 0 size


--loop that builds rows returns ([[array!!y!!x, getProx array x y size] : nextElements array x+1 y size)] - returns 2d array of [isBomb, proximityCount, visibilityDesignator] pairs
proxLoopRow :: [[Int]] -> Int -> Int -> Int -> [[Int]]
proxLoopRow array x y size
  | x == size-1 = [[((array!!y)!!x),(getProx array x y size), 0]] --base case, when done with a row, return the full row
  | otherwise = [((array!!y)!!x),(getProx array x y size), 0] : (proxLoopRow array (x+1) y size)

--loop that compiles rows [buildRow array x y size : compileRows array x y+1 size] - returns 3d array
proxLoopCol :: [[Int]] -> Int -> Int -> [[[Int]]]
proxLoopCol array y size
  | y == size-1 = [proxLoopRow array 0 y size] --base case, when done with a row, return the full row
  | otherwise = (proxLoopRow array 0 y size) : (proxLoopCol array (y+1) size)


--params = [[Isbomb]], row, col, sizeOfGrid
--returns the number of bombs in proximity
getProx :: [[Int]] -> Int -> Int -> Int -> Int
getProx array x y size
  | (y == 0 || y == size-1) = if (y==0) --if top
                           then do
                               if (x == 0 || x == size-1) --if its on either of the extremes x
                               then do
                                   if(x==0) -- if top left
                                       then ((array!!y)!!(x+1))+((array!!(y+1))!!(x+1))+((array!!(y+1))!!(x))
                                   else --if top right
                                       ((array!!y)!!(x-1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))
                               else --if its in the top middle
                                   ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y+1))!!(x+1))
                       else
                           do
                               if (x == 0 || x == size-1) --if its on either of the extremes x
                               then do
                                   if(x==0) -- if bottom left
                                       then ((array!!y)!!(x+1))+((array!!(y-1))!!(x+1))+((array!!(y-1))!!(x))
                                   else --if bottom right
                                       ((array!!y)!!(x-1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))
                               else --if its in the bottom middle
                                   ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))+((array!!(y-1))!!(x+1))
  | (x == 0 || x == size-1) = do
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


printField2D :: Int -> Int -> Int -> String
printField2D x y size
  | x == 0 && y/= size+1 = concat [getCoordinateY y, " ", printField2D (x+1) (y) size]
  | x == size && y==0 = concat [getCoordinateX x, ['\n'], printField2D 0 (y+1) size]
  | x == size && y/=0 && y/=size+1 = concat ["?", ['\n'], printField2D 0 (y+1) size]

  | y == 0 && x/=size = concat [getCoordinateX x, " ", printField2D (x+1) y size]
  | y==size+1 = ""
  | otherwise = concat ["?", " ", printField2D (x+1) y size]

getCoordinateX :: Int -> String
getCoordinateX x =
    [" ", "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","!","@","#","$"]!!x

getCoordinateY :: Int -> String
getCoordinateY y =
    [" ","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"]!!y


--used during game to print currently known board
getPrintableCharacter :: [Int] -> String
getPrintableCharacter array
  | array!!2 == 0 = "?" -- lets say 0 is specifier for unknown/unflag
  | array!!2 == 1 = "X" -- lets say 1 is specifier for flag
  | otherwise = show (array!!1) -- where m is the index for the proximity count


-- allows creates a singular string from the completed 3d array
printField3D :: [[[Int]]] -> Int -> Int -> Int -> String
printField3D array x y size
  | x == size-1 && y /= size = concat [getPrintableCharacter ((array!!y)!!x), ['\n'], printField3D array 0 (y+1) size]
  | y==size = ""
  | otherwise = concat [getPrintableCharacter ((array!!y)!!x), " ", printField3D array (x+1) y size]


 --- END GAME -----

--used at the end of the game to print all areas
getPrintableCharacterEnd :: [Int] -> String
getPrintableCharacterEnd array
  | array!!0 == 0 = show (array!!1) -- if its not a bomb, then print proximity
  | otherwise = "B" -- if it is a bomb, print B


--call when the game is over
printField3DComplete :: [[[Int]]] -> Int -> Int -> Int -> String
printField3DComplete array x y size
  | x == size-1 && y /= size = concat [getPrintableCharacterEnd ((array!!y)!!x), ['\n'], printField3DComplete array 0 (y+1) size]
  | y==size = ""
  | otherwise = concat [getPrintableCharacterEnd ((array!!y)!!x), " ", printField3DComplete array (x+1) y size]


-- ============================================



-- =================================== --
-- Array Helper functions
-- ================================== --

--takes x,y and the array to give back the tuple (bomb, proximity, visibilityDesignator)
getPositionTuple :: Int -> Int -> [[[Int]]] -> [Int]
getPositionTuple x y array =
    (array!!y)!!x

getIsBomb :: Int -> Int -> [[[Int]]] -> Bool
getIsBomb x y array =
    (getPositionTuple x y array)!!0 == 1

{-
-- example
getIsBombEfficient :: [[[Int]]] -> Int -> Int -> Bool
getIsBombEfficient array x y =
    1== ((array!!y)!!x)!!0-}

getProximity :: Int -> Int -> [[[Int]]] -> Int
getProximity x y array =
    (getPositionTuple x y array)!!1

--0 is 'unknown', 1 is 'flag', 2 is 'dug'
getVisibilityDesignator :: Int -> Int -> [[[Int]]] -> Int
getVisibilityDesignator x y array =
    (getPositionTuple x y array)!!2


-- fieldUpdate(array, x,y) - upadte a select position
    -- take array as reference
    -- return new array with visibilityDesignator (index 2), now set to value
--the current optional implementation specifies location, and uses 2 other values (currentX and currentY for recursive indexing)
fieldUpdate :: [[[Int]]] -> Int -> Int -> Int -> [[[Int]]]
fieldUpdate array x y action =
    fieldUpdateCol array x y action 0


fieldUpdateCol :: [[[Int]]] -> Int -> Int -> Int -> Int -> [[[Int]]]
fieldUpdateCol array x y action currentY =
    if(y==currentY)
        then fieldUpdateRow (head array) x action 0 : (tail array)
    else
        (head array) : fieldUpdateCol (tail array) x y action (currentY+1)

fieldUpdateRow :: [[Int]] -> Int -> Int -> Int -> [[Int]]
fieldUpdateRow array x action currentX =
    if(x==currentX)
        then
            [((array!!x)!!0), ((array!!x)!!1), action] : (tail array)
    else
        (head array) : fieldUpdateRow (tail array) x action (currentX+1)

-- ============================================
