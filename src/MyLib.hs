module MyLib (getCalibrationValues) where

import Data.Char

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

convertToCalibrationValues :: [Char] -> [Char]
convertToCalibrationValues xs = head xs : [last xs]

getCalibrationValues :: [[Char]] -> [[Char]]
getCalibrationValues xs = map convertToCalibrationValues $ getDigitsInEntries xs
