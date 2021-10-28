module MyLib (someFunc) where

import Cows (randomCow)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom (DevRandom(DevURandom))
import qualified Data.Text.IO as TIO


someFunc :: IO ()
someFunc = TIO.putStrLn "I am not a cow"
