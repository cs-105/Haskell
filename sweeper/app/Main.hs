module Main where

--import qualified MyLib (someFunc)

--import System.Random
import System.Console.ANSI

import Data.List (findIndices, delete)
import Data.String (String)
import Minefield (getIndexInRange, allLocations, initializeBombArray, proxLoop)
import Solver (solverMain, getVisible, isValidFieldInitial)
-- =================================== --
-- CONST Testing
-- ================================== --

-- ================================== --



-- =================================== --
-- IO & managment for MineSweeper
-- ================================== --

--TODO - DONE AFTER ALL OTHERS (dependent on others, so subject to most change)
--Main() - calls initial minesweeper builder and looper
main :: IO ()
main = do
    --generateMinefield
    ioDificultyLoop ""

--ioDificultyLoop() - (prints, waits, parses user input,)
--continually waits for correct user input, once it exists, moves on to ioLoopInitial (which waits for correct position input)
ioDificultyLoop :: String -> IO ()
ioDificultyLoop message = do
  putStrLn message
  --setSGR [SetColor Foreground Dull Black, SetColor Background Dull White] --testing

  putStrLn "Choose A Difficulty: (Defaults to easy)"
  putStrLn "Easy: 1   Intermediate: 2   Expert: 3     Custom: 4"

  response <- getLine

  let input = ((words response) !! 0)
  if input == "4" || input == "Custom"
      then parseSizeInput
  else ioInitial (difficultySwitch input) "Give a position ('X y') followed by an action ('dig'): "

--TODO - read the inputs to make sure they are valid BEFORE trying to read (can use parseIsInt)
parseSizeInput :: IO ()
parseSizeInput = do
    putStrLn "Define the paramaters: (an illegal input in any field will send you back upon completion)"
    putStrLn "How many rows (1-30)? "
    x <- getLine
    --parse to int, and double check its valid
    putStrLn "How many columns (1-16)?  "
    y <- getLine
    --parse to int, and double check its valid
    putStrLn "How many bombs (Positive number smaller than (x*y)-9)? "
    bombCount <- getLine
    --parse to int, and double check its valid
    let intX = read x
    let intY = read y
    let intCount = read bombCount
    if intX > 1 && intY > 1 && intCount >= 1 && intX <= 30 && intY <= 16 && (intCount < ((intX*intY)-9))
      then presetConfirmation ("Confirm preset: " ++ show intX ++"x" ++ show intY ++", " ++ show intCount ++ " bombs" ++ ['\n'] ++"[y/N]?") [intX, intY, intCount]
    else
      ioDificultyLoop "Invalid input"

--iterate over a string and determine whether it is actually a number
parseIsInt:: String -> Bool
parseIsInt [] = True
parseIsInt (letter:rest) =
  elem letter digits && parseIsInt rest

digits:: [Char]
digits = ['1','2','3','4','5','6','7','8','9','0']

presetConfirmation :: String -> [Int] -> IO ()
presetConfirmation message gamePreset = do
  putStrLn message
  response <- getLine
  if response == "y" || response == "Y"
    then ioInitial gamePreset messageContinue
  else ioDificultyLoop ("Returning to difficulty selection..."++['\n'])

messageContinue :: String
messageContinue = "Give a position ('X y') followed by an action ('flag', 'dig', 'unflag'); or type 'solve' to have a solver attempt a single solution: "

difficultySwitch :: String -> [Int]
difficultySwitch difficulty
  | difficulty == "1" = [9, 9, 10]
  | difficulty == "2" = [16, 16, 40]
  | difficulty == "3" = [30, 16, 99]
  | otherwise = [9, 9, 10]

--Used to get isolate the sizes from the game preset value
getSizes :: [Int] -> [Int]
getSizes array =
  [(array!!0),(array!!1)] --just return the x and y


--IOLoop() - (prints, waits, parses user input,)
    --continually waits for correct user input, once it exists, moves on to ioLoop
ioInitial :: [Int] -> String -> IO ()
ioInitial gamePreset message = do
    --putStrLn (printField2D 0 0 (gamePreset!!0) (gamePreset!!1))
    printField2DInitialIO [(gamePreset!!0),(gamePreset!!1)]
    putStrLn message

    position <- getLine

    let input = parseInput position
    --create initial array
    if ((length input == 3) && (input!!0)<(gamePreset!!0) && (input!!1) < (gamePreset!!1) && ((input!!2) == 2)) -- if valid input
    then do
        createBombArray input gamePreset
    else
        ioInitial gamePreset ("Invalid Input. "++ "Give a position ('X y') followed by an action ('dig'): " ++['\n','\t']++"For example, 'A a dig': ") --invalid input



--if there is actual bombs on the field
    -- build the array
    -- remove the intial user's chosen place from the list of possible bombs
    -- set the initial bomb array to empty (depricated unless testing proves a need)
    -- send the bombcount (gamepreset!!2) along 
createBombArray :: [Int] -> [Int] -> IO ()
createBombArray input gamePreset =
  if (gamePreset!!2 >0)
    then getBombs (removeFromPossibleBombsList (gatherNeighbors (input!!0) (input!!1)) (allLocations (gamePreset!!0) (gamePreset!!1))) (gamePreset!!2) [] input gamePreset
  else if((input!!2)==2) --if user is digging
      then ioLoop (scanInitial (proxLoop (initializeBombArray [] (getSizes gamePreset)) (getSizes gamePreset)) (input!!0) (input!!1) (getSizes gamePreset)) gamePreset messageContinue
      else ioLoop (fieldUpdate (proxLoop (initializeBombArray [] (getSizes gamePreset)) (getSizes gamePreset)) (input!!0) (input!!1) (input!!2)) gamePreset messageContinue


getBombs :: [[Int]] -> Int -> [[Int]] -> [Int] -> [Int] -> IO ()
getBombs possibleBombLocations bombCount currentBombArray input gamePreset = do
  x <- (getIndexInRange ((length possibleBombLocations)-1))
  if (bombCount > 0)
    then getBombs (delete (possibleBombLocations!!x) possibleBombLocations) (bombCount-1) ((possibleBombLocations!!x):currentBombArray) input gamePreset
  else
    if((input!!2)==2) --if user is digging
      then ioLoop (scanInitial (proxLoop (initializeBombArray currentBombArray (getSizes gamePreset)) (getSizes gamePreset)) (input!!0) (input!!1) (getSizes gamePreset)) gamePreset messageContinue -- putStr (show (isValidFieldInitial (getVisible (fieldUpdate (proxLoop (initializeBombArray currentBombArray (getSizes gamePreset)) (getSizes gamePreset)) (input!!0) (input!!1) (input!!2))) currentBombArray (gamePreset!!2)))
      else ioLoop (fieldUpdate (proxLoop (initializeBombArray currentBombArray (getSizes gamePreset)) (getSizes gamePreset)) (input!!0) (input!!1) (input!!2)) gamePreset messageContinue
--heretic
--isvalid (getVisible (initilize bombArray)) currentBombArray (gamePreset!!2)


gatherNeighbors :: Int -> Int -> [[Int]]
gatherNeighbors x y =  [[(x-1),(y-1)],[x,(y-1)],[(x+1),(y-1)],[(x-1),y],[x,y],[(x+1),y],[(x-1),(y+1)],[x,(y+1)],[(x+1),(y+1)]]

removeFromPossibleBombsList :: [[Int]] -> [[Int]] -> [[Int]]
removeFromPossibleBombsList [] possibleBombs = possibleBombs
removeFromPossibleBombsList notBombs [] = []
removeFromPossibleBombsList (currentNotBomb:notBombs) possibleBombs =
  removeFromPossibleBombsList notBombs (delete currentNotBomb possibleBombs)


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
ioLoop:: [[[Int]]] -> [Int] -> String -> IO ()
ioLoop array gamePreset message = do
    --putStrLn (show (length ((array!!0)!!0)))
    putStr ['\n','\n']
    printField3DInitialIO array (getSizes gamePreset)
    let gameStatus = currentGameData array --gameStatus!!0 is unknown count, gameStatus!!1 is flags count
    putStrLn (progressMessage gameStatus gamePreset)
    if ((gameStatus!!0 - gamePreset!!2) == 0)
      then putStrLn (['\n','\t']++"You Win")
    else do
      putStrLn message

      position <- getLine

      let input = parseInput position

      --win condition = (unknown count - bomb count), when reach 1 and digging not a bomb, game must be ending, flag count is just for help, bombCount is gamePreset!!

      --create initial array
      if validAction input array gamePreset -- if valid input/action
      then
          if (input!!2 ==2 ) && getIsBomb (input!!0) (input!!1) array --if the user is digging a bomb, end
              then do
                putStr ['\n','\n']
                printField3DEndInitialIO array (getSizes gamePreset)
                putStrLn (progressMessage gameStatus gamePreset)
                setSGR [SetColor Background Vivid Red, SetColor Foreground Vivid White]
                putStr (['\t'] ++ "Game Over")
                setSGR [Reset]
          else if(input!!2==2)
                  then ioLoop (scanInitial array (input!!0) (input!!1) (getSizes gamePreset)) gamePreset messageContinue
              else ioLoop (fieldUpdate array (input!!0) (input!!1) (input!!2)) gamePreset messageContinue

      --check if its invalid, or if player is using solver
      else
        if input == [1] then do
            let h = solve array (gamePreset!!2)
            let msg = ("Solver tried: "++ (rowKeyArray!!(h!!0))++ " " ++(colKeyArray!!(h!!1)) ++ " "++ (actionKeyArray!!(h!!2) ++ " with a " ++ (show (h!!3)) ++ "/" ++ (show (h!!4))) ++ " chance.")
            if (h!!2 ==2 ) && getIsBomb (h!!0) (h!!1) array --if the user is digging a bomb, end
              then do
                putStr ['\n','\n']
                printField3DEndInitialIO array (getSizes gamePreset)
                putStrLn (progressMessage gameStatus gamePreset)
                setSGR [SetColor Background Vivid Red, SetColor Foreground Vivid White]
                putStr (['\t'] ++ "Game Over")
                setSGR [Reset]
            else if(h!!2==2)
                    then ioLoop (scanInitial array (h!!0) (h!!1) (getSizes gamePreset)) gamePreset (msg ++ ['\n']++ messageContinue)
                else ioLoop (fieldUpdate array (h!!0) (h!!1) (h!!2)) gamePreset (msg ++ ['\n']++ messageContinue)
        else
          ioLoop array gamePreset ("Invalid Input. " ++ messageContinue ++['\n','\t']++"For example, 'A a flag': ")--invalid input

solve :: [[[Int]]] -> Int -> [Int] --MONTY PUT THE SOLVER HERE
solve field bmombCount = solverMain field bmombCount
--solve field bmombCount = do
  --let x = solveLoopInitial (getVisible field)
  --putStrLn ((show (length x))++" "++(show (length (x!!0)))++ " "++(show (length (x!!1))))
  --return [0,1,2,3,4]
--solve field bmombCount = [0,1,2,3,4]





--solve takes numOfBombs (gamePreset!!2), arrayOfFlagPositions, arrayOfUnknownsNeighboringKnowns 

progressMessage :: [Int] -> [Int] ->  String
progressMessage statusCounts gamePreset =
  "Safe Positions left: "++ (show ((statusCounts!!0)-(gamePreset!!2))) ++ ['\n'] ++ "Flag count: " ++ (show (statusCounts!!1))

--validAction() - take [x,y,action] and the field
    -- Verify
        -- Is the position actually within bounds? 
        -- Is the region available for more actions (not already dug)?
validAction :: [Int] -> [[[Int]]] -> [Int] -> Bool
validAction parsedInput field size =
    ((length parsedInput) == 3 && (parsedInput!!0) < (size!!0) && (parsedInput!!1) < (size!!1)) && ((getVisibilityDesignator (parsedInput!!0) (parsedInput!!1) field)/=2)



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
        then if (elem (l!!0) rowKeyArray && elem (l!!1) colKeyArray && elem (l!!2) actionKeyArray)
            then
                [(findIndices (==(l!!0)) rowKeyArray)!!0, (findIndices (==(l!!1)) colKeyArray)!!0, (findIndices (==(l!!2)) actionKeyArray)!!0]
            else []
    else
      if (length l == 1) && ((l!!0) == "solve" || (l!!0) == "help") then [1] else [] --Not following correct convention for input


--in expert mode, there are only (at max 16 rows, so we start there)
colKeyArray :: [String]
colKeyArray = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"]

--in expert mode, there are only (at max 16 rows, so we start there)
rowKeyArray :: [String]
rowKeyArray = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","!","@","#","$"]

--in expert mode, there are only (at max 16 rows, so we start there)
actionKeyArray :: [String]
actionKeyArray = ["unflag","flag","dig"]


-- ============================================

-- =================================== --
-- Array IO Helper functions
-- ================================== --

--Hardcoded arrays - Allows for quick indexing and creation of the UI Coordinates
getCoordinateX :: Int -> String
getCoordinateX x =
    [" ", "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","!","@","#","$"]!!x
getCoordinateY :: Int -> String
getCoordinateY y =
    [" ","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"]!!y


--finds what a single point should be represented as when printed - (used during game to print currently known board)
getPrintableCharacter :: [Int] -> String
getPrintableCharacter array
  | array!!2 == 0 = "?" -- lets say 0 is specifier for unknown/unflag
  | array!!2 == 1 = "X" -- lets say 1 is specifier for flag
  | otherwise = show (array!!1) -- where m is the index for the proximity count, because it would be visibile if it were a bomb


--Prints the UI spread for the user's intial choice (DOES NOT CREATE A FIELD)
printField2DInitialIO :: [Int] -> IO ()
printField2DInitialIO size = printField2DIO 0 0 (size!!0) (size!!1)

--Prints the UI spread for the user's intial choice (DOES NOT CREATE A FIELD)
printField2DIO :: Int -> Int -> Int -> Int -> IO ()
printField2DIO x y sizeX sizeY
  | x == 0 && y/= sizeY+1 = do
    setSGR coordinateColor
    putStr (getCoordinateY y)
    putStr " "
    setSGR [Reset]
    printField2DIO (x+1) y sizeX sizeY
  | x == sizeX && y==0 = do
    setSGR coordinateColor
    putStr (getCoordinateX x)
    setSGR [Reset]
    putStr ['\n']
    printField2DIO 0 (y+1) sizeX sizeY
  | x == sizeX && y/=0 && y/=sizeY+1 = do
    setSGR (whatColor "?")
    putStr "?"
    setSGR [Reset]
    putStr ['\n']
    printField2DIO 0 (y+1) sizeX sizeY
  | y == 0 && x/=sizeX = do
    setSGR coordinateColor
    putStr ((getCoordinateX x) ++ " ")
    setSGR [Reset]
    printField2DIO (x+1) y sizeX sizeY
  | y==sizeY+1 = putStr ""
  | otherwise = do
    setSGR (whatColor "?")
    putStr "? "
    setSGR [Reset]
    printField2DIO (x+1) y sizeX sizeY

printField3DInitialIO :: [[[Int]]] -> [Int] -> IO ()
printField3DInitialIO array sizes = printField3DIO array 0 0 (sizes!!0) (sizes!!1)


-- creates a singular formated string from the completed 3d array
printField3DIO :: [[[Int]]] -> Int -> Int -> Int -> Int -> IO ()
printField3DIO array x y sizeX sizeY
  | x == 0 && y/=sizeY+1 = do
      setSGR coordinateColor
      putStr (getCoordinateY y)
      putStr " "
      setSGR [Reset]
      printField3DIO array (x+1) y sizeX sizeY
  | x == sizeX && y==0 = do
      setSGR coordinateColor
      putStr (getCoordinateX x)
      setSGR [Reset]
      putStr ['\n']
      printField3DIO array 0 (y+1) sizeX sizeY
  | x == sizeX && y/=0 && y/=sizeY+1 = do
      setSGR (whatColor (getPrintableCharacter ((array!!(y-1))!!(x-1))))
      putStr (getPrintableCharacter ((array!!(y-1))!!(x-1)))
      setSGR [Reset]
      putStr ['\n']
      printField3DIO array 0 (y+1) sizeX sizeY
  | y == 0 && x/=sizeX = do
      setSGR coordinateColor
      putStr (getCoordinateX x)
      putStr " "
      setSGR [Reset]
      printField3DIO array (x+1) y sizeX sizeY
  | y==sizeY+1 = putStr ""
  | otherwise = (do
      setSGR (whatColor (getPrintableCharacter ((array!!(y-1))!!(x-1))))
      putStr ((getPrintableCharacter ((array!!(y-1))!!(x-1))) ++ " ")
      setSGR [Reset]
      printField3DIO array (x+1) y sizeX sizeY)

whatColor :: String -> [SGR]
whatColor visibleChar
  | visibleChar=="X" = [SetColor Foreground Dull Red, SetColor Background Dull Green]
  | visibleChar=="?" = [SetColor Foreground Dull Black, SetColor Background Dull Green]
  | visibleChar=="B" = [SetColor Foreground Dull Black, SetColor Background Dull Red]
  | visibleChar=="0" = [SetColor Foreground Dull Blue, SetColor Background Dull Green]
  | otherwise = [SetColor Foreground Dull Black, SetColor Background Dull Green]

coordinateColor :: [SGR]
coordinateColor = [SetColor Foreground Dull Black, SetColor Background Dull Blue]


 --v-v-v- END GAME -v-v-v-v-

--at the end of the game there are only 2 visible options (bomb or proximity count)
getPrintableCharacterEnd :: [Int] -> String
getPrintableCharacterEnd array
  | array!!0 == 0 = show (array!!1) -- if its not a bomb, then print proximity
  | otherwise = "B" -- if it is a bomb, print B



--call when the game is over (creates a single formated string with full visibility)
printField3DEndInitialIO :: [[[Int]]] -> [Int] -> IO ()
printField3DEndInitialIO array sizes = printField3DEndIO array 0 0 (sizes!!0) (sizes!!1)

--call when the game is over (creates a single formated string with full visibility)
printField3DEndIO :: [[[Int]]] -> Int -> Int -> Int -> Int -> IO ()
printField3DEndIO array x y sizeX sizeY
  | x == 0 && y/=sizeY+1 = do
      setSGR coordinateColor
      putStr (getCoordinateY y)
      putStr " "
      setSGR [Reset]
      printField3DEndIO array (x+1) y sizeX sizeY
  | x == sizeX && y==0 = do
      setSGR coordinateColor
      putStr (getCoordinateX x)
      setSGR [Reset]
      putStr ['\n']
      printField3DEndIO array 0 (y+1) sizeX sizeY
  | x == sizeX && y/=0 && y/=sizeY+1 = do
      setSGR (whatColor (getPrintableCharacterEnd ((array!!(y-1))!!(x-1))))
      putStr (getPrintableCharacterEnd ((array!!(y-1))!!(x-1)))
      setSGR [Reset]
      putStr ['\n']
      printField3DEndIO array 0 (y+1) sizeX sizeY
  | y == 0 && x/=sizeX = do
      setSGR coordinateColor
      putStr (getCoordinateX x)
      putStr " "
      setSGR [Reset]
      printField3DEndIO array (x+1) y sizeX sizeY
  | y==sizeY+1 = putStr ""
  | otherwise = (do
      setSGR (whatColor (getPrintableCharacterEnd ((array!!(y-1))!!(x-1))))
      putStr ((getPrintableCharacterEnd ((array!!(y-1))!!(x-1))) ++ " ")
      setSGR [Reset]
      printField3DEndIO array (x+1) y sizeX sizeY)


-- ============================================



-- =================================== --
-- Array IO Helper functions
-- ================================== --

--takes x,y and the array to give back the tuple (bomb, proximity, visibilityDesignator)
getPositionTuple :: Int -> Int -> [[[Int]]] -> [Int]
getPositionTuple x y array =
    (array!!y)!!x

--takes the field and the coordinate to tell if its a bomb
getIsBomb :: Int -> Int -> [[[Int]]] -> Bool
getIsBomb x y array =
    (getPositionTuple x y array)!!0 == 1


--takes the field and the coordinate to tell the proximity count
getProximity :: Int -> Int -> [[[Int]]] -> Int
getProximity x y array =
    (getPositionTuple x y array)!!1


--takes the field and the coordinate to tell the proximity count
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

--gets to the correct row
fieldUpdateCol :: [[[Int]]] -> Int -> Int -> Int -> Int -> [[[Int]]]
fieldUpdateCol (first:rest) x y action currentY =
    if y==currentY
        then fieldUpdateRow first x action 0 : rest
    else
        first : fieldUpdateCol rest x y action (currentY+1)
--gets to the correct element in the row, and updates
fieldUpdateRow :: [[Int]] -> Int -> Int -> Int -> [[Int]]
fieldUpdateRow (first:rest) x action currentX =
    if x==currentX
        then
            [(first!!0), (first!!1), action] : rest
    else
        first : fieldUpdateRow rest x action (currentX+1)


-- =================================== --
-- Accumulation of current game data (flags, invisible spaces)
-- =================================== --
currentGameData :: [[[Int]]] -> [Int]
currentGameData (row:restOfArray) =
  if null restOfArray
    then currentGameDataRow row
  else do
    let value = currentGameDataRow row
    let otherValue = currentGameData restOfArray
    [(value!!0)+(otherValue!!0),(value!!1)+(otherValue!!1)]

currentGameDataRow :: [[Int]] -> [Int]
currentGameDataRow [] = [0,0]
currentGameDataRow (element:restOfRow) =
  if null restOfRow
    then currentGameDataPosition element
  else do
    let value = currentGameDataPosition element
    let otherValue = currentGameDataRow restOfRow
    [(value!!0)+(otherValue!!0),(value!!1)+(otherValue!!1)]

currentGameDataPosition :: [Int] -> [Int]
currentGameDataPosition positionValues
  | (positionValues!!2) == 0 = [1,0]  --0 is 'unknown'
  | (positionValues!!2) == 1 = [1,1]  --1 is 'flag', also means undug
  | otherwise = [0,0]
-- ============================================




-- =================================== --
-- Scanning Functions (reveal area)
-- ================================== --

scanInitial :: [[[Int]]] -> Int -> Int -> [Int] -> [[[Int]]]
scanInitial field x y size = scanUpdateLoop field (scanShouldVisible field [] x y (size!!0) (size!!1))

scanUpdateLoop :: [[[Int]]] -> [[Int]] -> [[[Int]]]
scanUpdateLoop field (position:rest) =
    if rest == []
        then (fieldUpdate field (position!!0) (position!!1) 2)
    else scanUpdateLoop (fieldUpdate field (position!!0) (position!!1) 2) rest

--take the initial positon, then recursively add surrounding positions that should be visible (can then be fed as a loop to fieldUpdate)
    --base case = [] empty array when the position should not be made visible (if it has already been made visible)
    --second base case = [x,y] when the position has a prox > 0
    --recursive loop = [x,y] : recursion of each position (L,TL,T,TR,R,BR,B,BL)
scanShouldVisible :: [[[Int]]] -> [[Int]] ->Int -> Int -> Int -> Int -> [[Int]]
scanShouldVisible field positions x y sizeX sizeY
  | getVisibilityDesignator x y field == 2 = positions
  | elem [x,y] positions = positions
  | getProximity x y field > 0 = [x,y]:positions
  | otherwise = scanSurrounding field positions x y sizeX sizeY

--goes through each possible condtion to ensure no indexing errors
scanSurrounding :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurrounding field positions x y xSize ySize
  | elem [x,y] positions = positions                         --if already contains, return
  | x == xSize-1 && y==0 = scanSurroundingTR field ([x,y]:positions) x y xSize ySize       --top right - scan L,LB,B
  | x == 0 && y==0 = scanSurroundingTL field ([x,y]:positions) x y xSize ySize              --top left - scan R,RB,B
  | y == 0 = scanSurroundingT field ([x,y]:positions) x y xSize ySize                       --top middle - scan L,LB,B,RB,R
  | x == xSize-1 && y==ySize-1 = scanSurroundingBR field ([x,y]:positions) x y xSize ySize  --bottom right - scan T,TL,L
  | x == 0 && y==ySize-1 = scanSurroundingBL field ([x,y]:positions) x y xSize ySize        --bottom left - scan T,TR,R
  | y==ySize-1 = scanSurroundingB field ([x,y]:positions) x y xSize ySize                   --bottom middle - scan L,TL,T,TR,R
  | x == 0 = scanSurroundingL field ([x,y]:positions) x y xSize ySize                       --left - scan T,TR,R,BR,B
  | x == xSize-1 = scanSurroundingR field ([x,y]:positions) x y xSize ySize                 --right - scan T,TL,L,BL,B
  | otherwise = scanSurroundingM field ([x,y]:positions) x y xSize ySize                    --middle/center - scan T,TR,R,BR,B,BL,L,TL


--Key for Future area debugging
--    (scanShouldVisible field positions (x+1) y)         --R
--    (scanShouldVisible field positions (x+1) (y+1))     --BR
--    (scanShouldVisible field positions x (y+1))         --B
--    (scanShouldVisible field positions (x-1) (y+1))     --BL
--    (scanShouldVisible field positions (x-1) y)         --L
    -- (scanShouldVisible field positions (x-1) (y-1))     --TL
    -- (scanShouldVisible field positions x (y-1))         --T
    -- (scanShouldVisible field positions (x+1) (y-1))     --TR


scanSurroundingM :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingM field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions (x+1) (y-1) xSize ySize) x (y-1)  xSize ySize)  (x-1) (y-1)  xSize ySize)  (x-1) y xSize ySize) (x-1) (y+1) xSize ySize) x (y+1) xSize ySize) (x+1) (y+1) xSize ySize) (x+1) y xSize ySize)

scanSurroundingTR :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingTR field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions (x-1) y xSize ySize)  (x-1) (y+1) xSize ySize)  x (y+1) xSize ySize)

scanSurroundingTL :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingTL field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions x (y+1) xSize ySize) (x+1) (y+1) xSize ySize) (x+1) y xSize ySize)


scanSurroundingL :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingL field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions (x+1) (y-1) xSize ySize) x (y-1) xSize ySize) x (y+1) xSize ySize) (x+1) (y+1) xSize ySize) (x+1) y xSize ySize)


scanSurroundingR :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingR field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions x (y-1) xSize ySize) (x-1) (y-1) xSize ySize) (x-1) y xSize ySize) (x-1) (y+1) xSize ySize) x (y+1) xSize ySize)

scanSurroundingB :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingB field positions x y xSize ySize =
   (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions (x+1) (y-1) xSize ySize) x (y-1) xSize ySize) (x-1) (y-1) xSize ySize) (x-1) y xSize ySize) (x+1) y xSize ySize)


scanSurroundingBL :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingBL field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions (x+1) (y-1) xSize ySize) x (y-1) xSize ySize) (x+1) y xSize ySize)

scanSurroundingBR :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingBR field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions x (y-1) xSize ySize) (x-1) (y-1) xSize ySize) (x-1) y xSize ySize)


scanSurroundingT :: [[[Int]]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
scanSurroundingT field positions x y xSize ySize =
    (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field (scanShouldVisible field positions (x-1) y xSize ySize) (x-1) (y+1) xSize ySize) x (y+1) xSize ySize) (x+1) (y+1) xSize ySize) (x+1) y xSize ySize)


-- ============================================
