module MyLib (getCalibrationValues) where

import Data.Char

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

convertToCalibrationValues :: [Char] -> [Char]
convertToCalibrationValues xs =
  let 
    h = [head xs]
    l = [last xs]
  in 
    h ++ l
getCalibrationValues :: [[Char]] -> [[Char]]
getCalibrationValues xs = map convertToCalibrationValues $ getDigitsInEntries xs
