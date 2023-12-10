module Main where

import qualified MyLib (getCorrectedCalibrationValues)

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  -- print $ lines dt
  -- print $ MyLib.getCorrectedCalibrationValues $ lines dt
  print $ sum $ MyLib.getCorrectedCalibrationValues $ lines dt
  
