
-- =================================== --
-- IO & managment for MineSweeper
-- ================================== --

--Main() - calls initial minesweeper builder and looper
    --print empty array
    --looper(intialBuidl(take input))

--Looper() - (prints, waits, and parses user input)
    --Print the passed Array
    --Parses input (test whether better to do string operations or multiple seperate inputs)
    --Sends input to mineFieldUpdate
    --Send to the Looper a Bool (isDone?) and the updated Array

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

-- loop :: [[int]] -> Int -> Int -> Int -> [[[int]]]
-- loop [] x y size = [[[]]]
-- loop (element:restOfArray) x y size = 
    --check for empty
    --append getProx of given index to the new array to create a 3d array filled with rows, cols, and [prox, isBomb]


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

-- checkIsBomb(array, x, y) - to quickly check if space is a bomb

-- revealLocation(array, x,y) - take array as reference return new array with isVisible true as all spaces

-- revealAll(array) - may be used if player hits a bomb - take array as reference and change all elements to visible