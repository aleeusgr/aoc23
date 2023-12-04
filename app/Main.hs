module Main where

import qualified MyLib (getCalibrationValues)

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ MyLib.getCalibrationValues $ lines dt
  
