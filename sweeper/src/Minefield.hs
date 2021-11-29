module Minefield (generateMinefield) where
import GhcPrelude (Enum)
import Data.List (permutations)

import System.Random ()


generateMinefield :: IO ()
generateMinefield = putStrLn "Please input a seed (positive integer):  "

-- generateMinefield :: (Num a, Enum a) => a -> a -> Int -> Int -> IO [[a]]
-- generateMinefield xSize ySize bombs seed = do
--     putStrLn "Please input a seed (positive integer):  "
--     y <- getLine
--     let intY = read y
--     safeGenerateMinefield xSize ySize bombs 5

safeGenerateMinefield :: Int -> Int -> Int -> Int -> [[Int]]
safeGenerateMinefield xSize ySize bombs seed = nBombsFromPool (shuffleBombs (allLocations xSize ySize) seed) bombs

allLocations :: Int -> Int -> [[Int]]
allLocations xSize ySize = [[x,y] | x <- [0..xSize-1], y <- [0..ySize-1]]

nBombsFromPool :: [[Int]] -> Int -> [[Int]]
nBombsFromPool potentialBombs n
    | n <= 1    = [head potentialBombs]
    | otherwise = [head potentialBombs] ++ (nBombsFromPool (tail potentialBombs) (n - 1))

shuffleBombs :: [[Int]] -> Int -> [[Int]]
shuffleBombs bombPool seed = permutations bombPool !! seed
    -- do
    --     putStrLn "Please input a seed (positive integer):  "
    --     y <- getLine
    --     let intY = read y
    --     permutations bombPool !! 1


-- when this is all done, generateMinefield will be as follows:
-- generateMinefield xSize ySize bombs = nBombsFromPool (nBombsFromPool (allLocations xSize ySize) bombs)


    -- do
    --     canvas <- [(i,j) | i <- [0,a], j <- [0,b]]
    --     putStr . show =<< randomRIO (a, b :: Int)