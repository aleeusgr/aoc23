module Main where

import qualified MyLib (getCorrectedCalibrationValues)

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ sum $ MyLib.getCorrectedCalibrationValues $ lines dt
  
