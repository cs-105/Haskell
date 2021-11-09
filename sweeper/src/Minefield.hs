module Minefield (generateMinefield) where
import GhcPrelude (Enum)

-- import System.Random

generateMinefield :: IO ()
generateMinefield = putStrLn "dog"

allLocations :: (Num a, Enum a) => a -> a -> [(a, a)]
allLocations xSize ySize = [(x,y) | x <- [0..xSize-1], y <- [0..ySize-1]]


    -- do
    --     canvas <- [(i,j) | i <- [0,a], j <- [0,b]]
    --     putStr . show =<< randomRIO (a, b :: Int)