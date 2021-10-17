
-- =================================== --
-- IO & managment for MineSweeper
-- ================================== --

--Main - calls initial minesweeper builder and looper
    --Requires Random Generation, Array builder/editor, UI Looper 

--Looper - prints, waits, and parses user input
    --Print the passed Array
    --Parses input (test whether better to do string operations or multiple seperate inputs)
    --Sends input to mineFieldUpdate
    --Send to the Looper a Bool (isDone?) and the updated Array

--MineField - 3d array of row, col, params -> [[[isVisible, ProximityCount, isBomb], [isVisible, ProximityCount, isBomb], ...]] - where isVisible has 3 options, yes, no, flag
-- ============================================




-- =================================== --
--Initial MINE FIELD REQUIREMENTS
-- ================================== --

--initialBuild - creates the array 
    --get an empty array - either hard code or loop
    --return populateWithBombs(bombNum, emptyArray, initialPosition)

--populateWithBombs ()
    --generate a random position (an x and a y, or go linear)
    --if the count is still not 0
        --if the position doesnt already have a bomb in it (and is not the initial guess), update the array 
            --(increase surrounding proximities by 1, and change isBomb to true)
            --feed it back into the function until appropriate number of bombs exist
        --else populateWithBombs(bombNum, array, initialPosition)
    --else
        --return populateProximitiesXPos(array)
 
--populateProximitiesXPos () -- uses isBomb array as reference
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

-- ============================================


-- =================================== --
-- Array Helper functions
-- ================================== --

-- checkIsBomb(array, x, y) - to quickly check if space is a bomb

-- revealLocation(array, x,y) - take array as reference return new array with isVisible true as all spaces

-- revealAll(array) - may be used if player hits a bomb - take array as reference and change all elements to visible