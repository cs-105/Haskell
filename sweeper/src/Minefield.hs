module Minefield (getIndexInRange, allLocations, initializeBombArray, proxLoop) where
--import Data.List (permutations)

import System.Random ( mkStdGen, getStdRandom, randomR )

-- ==========================
  -- Random Bomb Position Generation
-- ==========================
getIndexInRange :: Int -> IO Int
getIndexInRange largerBound =
  getStdRandom (randomR (0, largerBound))

--create a list of possible locations
allLocations :: Int -> Int -> [[Int]]
allLocations xSize ySize = [[x,y] | x <- [0..xSize-1], y <- [0..ySize-1]]

-- =========================


-- =================================== --
--Initial BOMB FIELD REQUIREMENTS
-- ================================== --

--here's an example distirbution of 10 bombs 
example9x9BombPositions :: [[Int]]
example9x9BombPositions = [[0,0],[1,5],[1,8],[3,5],[3,6],[3,7],[5,0],[5,8],[7,6],[8,0]] --Aa,Bf,


-- initializeBombArray() -- Use an array of bomb positions to construct a 2d array
    -- Use a passed array of bomb positions
    -- Loop - create initial array
        -- Check if the current position is a bomb
            -- If so, set position to 1
            -- Otherwise, 0
        -- nextPosition
    -- Return the created array
initializeBombArray :: [[Int]] -> [Int] -> [[Int]]
initializeBombArray bombPositions size =
    bombLoopCol bombPositions 0 (size!!0) (size!!1)

--loop that builds rows returns ([[array!!y!!x, getProx array x y size] : nextElements array x+1 y size)] - returns 2d array of [isBomb, proximityCount, visibilityDesignator] pairs
bombLoopRow :: [[Int]] -> Int -> Int -> Int ->[Int]
bombLoopRow bombPositions x y sizeX
  | x == sizeX-1 = if elem [x,y] bombPositions then [1] else [0]--base case, when done with a row, return the full row
  | otherwise = if elem [x,y] bombPositions then 1 : bombLoopRow bombPositions (x+1) y sizeX else 0 : bombLoopRow bombPositions (x+1) y sizeX

--loop that compiles rows [buildRow array x y size : compileRows array x y+1 size] - returns 3d array
bombLoopCol :: [[Int]] -> Int -> Int -> Int -> [[Int]]
bombLoopCol bombPositions y sizeX sizeY
  | y == sizeY-1 = [bombLoopRow bombPositions 0 y sizeX] --base case, when done with a row, return the full row
  | otherwise = (bombLoopRow bombPositions 0 y sizeX) : (bombLoopCol bombPositions (y+1) sizeX sizeY)

-- ========================================



-- =================================== --
-- 3D Field builders
-- ================================== --

--  MineField defined - 3d array of row, col, params -> [[[isVisible, ProximityCount, isBomb], [isVisible, ProximityCount, isBomb], ...]] - where isVisible has 3 options, unknown, flag, dug

--takes the initial 2d bomb array, and the size (square)
--returns a 3d array filled with cols, rows, [isBomb, proximity, visibilityDesignator]
proxLoop :: [[Int]] -> [Int] -> [[[Int]]]
proxLoop array size =
  proxLoopCol array 0 (size!!0) (size!!1)


--loop that builds rows returns ([[array!!y!!x, getProx array x y size] : nextElements array x+1 y size)] - returns 2d array of [isBomb, proximityCount, visibilityDesignator] pairs
proxLoopRow :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
proxLoopRow array x y sizeX sizeY
  | x == sizeX-1 = [[((array!!y)!!x),(getProx array x y sizeX sizeY), 0]] --base case, when done with a row, return the full row
  | otherwise = [((array!!y)!!x),(getProx array x y sizeX sizeY), 0] : (proxLoopRow array (x+1) y sizeX sizeY)

--loop that compiles rows [buildRow array x y size : compileRows array x y+1 size] - returns 3d array
proxLoopCol :: [[Int]] -> Int -> Int -> Int -> [[[Int]]]
proxLoopCol array y sizeX sizeY
  | y == sizeY-1 = [proxLoopRow array 0 y sizeX sizeY] --base case, when done with a row, return the full row
  | otherwise = (proxLoopRow array 0 y sizeX sizeY) : (proxLoopCol array (y+1) sizeX sizeY)


--params = [[Isbomb]], row, col, sizeOfGrid
--returns the number of bombs in proximity
getProx :: [[Int]] -> Int -> Int -> Int -> Int -> Int
getProx array x y sizeX sizeY
  | (y == 0 || y == sizeY-1) = if (y==0) --if top
                           then do
                               if (x == 0 || x == sizeX-1) --if its on either of the extremes x
                               then do
                                   if(x==0) -- if top left
                                       then ((array!!y)!!(x+1))+((array!!(y+1))!!(x+1))+((array!!(y+1))!!(x))
                                   else --if top right
                                       ((array!!y)!!(x-1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))
                               else --if its in the top middle
                                   ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y+1))!!(x+1))
                       else
                           do
                               if (x == 0 || x == sizeX-1) --if its on either of the extremes x
                               then do
                                   if(x==0) -- if bottom left
                                       then ((array!!y)!!(x+1))+((array!!(y-1))!!(x+1))+((array!!(y-1))!!(x))
                                   else --if bottom right
                                       ((array!!y)!!(x-1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))
                               else --if its in the bottom middle
                                   ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))+((array!!(y-1))!!(x+1))
  | (x == 0 || x == sizeX-1) = do
        if(x==0) -- if left
            then ((array!!y)!!(x+1))+((array!!(y+1))!!(x+1))+((array!!(y+1))!!(x))+((array!!(y-1))!!(x+1))+((array!!(y-1))!!(x))
        else --if right
            ((array!!y)!!(x-1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))
  | otherwise = --if its in the middle
        ((array!!y)!!(x-1))+((array!!y)!!(x+1))+((array!!(y+1))!!(x-1))+((array!!(y+1))!!(x))+((array!!(y+1))!!(x+1))+((array!!(y-1))!!(x-1))+((array!!(y-1))!!(x))+((array!!(y-1))!!(x+1))

-- ============================================





{- 
  generateMinefield :: [Int] -> [Int] -> IO ()
  generateMinefield notThisPosition gamePreset = putStrLn "Please input a seed (positive integer):  "


-- generateMinefield :: (Num a, Enum a) => a -> a -> Int -> Int -> IO [[a]]
-- generateMinefield xSize ySize bombs seed = do
--     putStrLn "Please input a seed (positive integer):  "
--     y <- getLine
--     let intY = read y
--     safeGenerateMinefield xSize ySize bombs 5

safeGenerateMinefield :: Int -> Int -> Int -> Int -> [[Int]]
safeGenerateMinefield xSize ySize bombs seed = nBombsFromPool (shuffleBombs (allLocations xSize ySize) seed) bombs




nBombsFromPool :: [[Int]] -> Int -> [[Int]]
nBombsFromPool potentialBombs n
    | n <= 1    = [head potentialBombs]
    | otherwise = (head potentialBombs) : (nBombsFromPool (tail potentialBombs) (n - 1))

shuffleBombs :: [[Int]] -> Int -> [[Int]]
shuffleBombs bombPool seed = permutations bombPool !! (seed `mod` (length (permutations bombPool)))

-- when this is all done, generateMinefield will be as follows:
-- generateMinefield xSize ySize bombs = nBombsFromPool (nBombsFromPool (allLocations xSize ySize) bombs)
-}
