module Minefield (generateMinefield) where
import GhcPrelude (Enum)
import Data.List (permutations)

import System.Random

generateMinefield :: IO ()
generateMinefield = putStrLn "dog"

allLocations :: (Num a, Enum a) => a -> a -> [[a]]
allLocations xSize ySize = [[x,y] | x <- [0..xSize-1], y <- [0..ySize-1]]

nBombsFromPool :: (Num a, Enum a) => [[a]] -> Int -> [[a]]
nBombsFromPool potentialBombs n
    | n <= 1    = [head potentialBombs]
    | otherwise = [head potentialBombs] ++ (nBombsFromPool (tail potentialBombs) (n - 1))

shuffleBombs :: (Num a, Enum a) => [[a]] -> [[a]]
shuffleBombs bombPool = 
    do
        g <- newStdGen
        permutations bombPool !! (randomR (0, length bombPool) g)

-- when this is all done, generateMinefield will be as follows:
-- generateMinefield xSize ySize bombs = nBombsFromPool (nBombsFromPool (allLocations xSize ySize) bombs)


    -- do
    --     canvas <- [(i,j) | i <- [0,a], j <- [0,b]]
    --     putStr . show =<< randomRIO (a, b :: Int)