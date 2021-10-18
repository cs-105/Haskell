
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

-- getProx :: [Int] -> Int x (row) -> Int y (col) -> Int (sum)
-- if on (top || bottom)
-- 	if (left || right)
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