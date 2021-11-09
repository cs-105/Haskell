module Main where

--import qualified MyLib (someFunc)
import Data.List

import System.Random

main :: IO ()
main = do
  putStr . show =<< randomRIO (0, 100 :: Int)