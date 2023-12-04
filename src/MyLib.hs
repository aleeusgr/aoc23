module MyLib (getDigitsInEntries, convertToCalibrationValues) where

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
