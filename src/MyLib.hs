module MyLib (getCalibrationValues, getCorrectedCalibrationValues, findAndReplaceNumber, correctCalibrationValues) where

import Data.Char
import Data.List
import Data.Maybe

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

convertToCalibrationValues :: [Char] -> [Char]
convertToCalibrationValues xs = head xs : [last xs]

intList :: [String] -> [Int]
intList = map (read :: String -> Int)

getCalibrationValues :: [[Char]] -> [Int]
getCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries xs

data Digits = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Enum)

wordToDigit :: [Char] -> [Char]
wordToDigit x = case x of
    "one" -> "1"
    "two" -> "2"
    "three" -> "3"
    "four" -> "4"
    "five" -> "5"
    "six" -> "6"
    "seven" -> "7"
    "eight" -> "8"
    "nine" -> "9"
    "zero" -> "0"
    _ -> "err"

testVals2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

digs :: [String]
digs = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

findAndReplaceNumber :: [Char] -> [String] -> [Char]
findAndReplaceNumber tv [] = head tv : findAndReplaceNumber (tail tv) digs
findAndReplaceNumber tv digs =
    let dig = head digs
        l = length dig
     in if dig == take l tv
            then wordToDigit dig ++ drop l tv
            else findAndReplaceNumber tv (tail digs)

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

wordContainsNumberNames :: [Char] -> Bool
wordContainsNumberNames tv = any (isJust . (`findString` tv)) digs

correctCalibrationValues :: [Char] -> [Char]
correctCalibrationValues word =
      if wordContainsNumberNames word --any of the digs are found in the string.
            then correctCalibrationValues (findAndReplaceNumber word digs)
            else word

getCorrectedCalibrationValues :: [[Char]] -> [Int]
getCorrectedCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries (map correctCalibrationValues xs)
