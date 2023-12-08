module MyLib (getCalibrationValues, Digits(..), getCorrectedCalibrationValues) where

import Data.Char
import Data.List
import Data.Maybe

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

convertToCalibrationValues :: [Char] -> [Char]
convertToCalibrationValues xs = head xs : [last xs]

intList :: [String] -> [Int]
intList = map (read::String->Int)

getCalibrationValues :: [[Char]] -> [Int]
getCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries xs

testVals2 = ["two1nine" , "eightwothree" , "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

dt = head testVals2

data Digits = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Enum)

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

-- :: "two1nine" -> Digit -> "21nine"
replace :: (Show p, Enum p) => p -> [Char] -> [Char]
replace n str  =
  let
  digit = show (fromEnum n)
  word = map toLower (show n)
  pos = findString word str 
  result = if isJust pos then 
    take (fromJust pos) str ++ digit ++ drop (fromJust pos + length word) str
        else str 
  in  result

replaceAll :: [Char] -> [Char]
replaceAll str =
  let
  replaceZero = replace Zero str
  replaceOne = replace One replaceZero
  replaceTwo = replace Two replaceOne
  replaceThree = replace Three replaceTwo
  replaceFour = replace Four replaceThree
  replaceFive = replace Five replaceFour
  replaceSix = replace Six replaceFive
  replaceSeven = replace Seven replaceSix
  replaceEight = replace Eight replaceSeven
  in replace Nine replaceEight

getCorrectedCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries (map replaceAll xs)

-- fold the list of inputs on it fold the list of digits.
-- git the list of digits

-- :: "eightwothree" -> 8wothree" -> "8wo3"
-- :: "zoneight234" -> "z1ight234"
-- getCorrectedCalibrationValues :: [[Char]] -> [Int]
-- getCorrectedCalibrationValues  = findString $ replaceWordWithDigit
--
