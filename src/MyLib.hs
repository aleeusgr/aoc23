module MyLib (getCalibrationValues) where

import Data.Char
import Data.List

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

convertToCalibrationValues :: [Char] -> [Char]
convertToCalibrationValues xs = head xs : [last xs]

intList :: [String] -> [Int]
intList = map (read::String->Int)

getCalibrationValues :: [[Char]] -> [Int]
getCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries xs

myWords :: [String]
myWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "zero"]

testVals2 :: [String]
testVals2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four",
             "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

-- tests with:
-- findString (myWords !! 1) (head testVals2)
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)


-- :: "eighttwothree" -> "8twothree" -> "823"
-- :: "zoneight234" -> "z1ight234"
-- getCorrectedCalibrationValues :: [[Char]] -> [Int]
-- getCorrectedCalibrationValues  = findString $ replaceWordWithDigit
--
