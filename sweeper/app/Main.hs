module Main where

--import qualified MyLib (someFunc)
import Data.List

import System.Random

main :: IO ()
main = do
  putStr . show =<< randomRIO (1, 6 :: Int)
  -- the above line simulated a die roll, generating an Int between 1 and 6 inclusive