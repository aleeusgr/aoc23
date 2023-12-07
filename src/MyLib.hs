module MyLib (getCalibrationValues, Digits(..), replace, findString) where

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
-- "one" "two"  "three"  "four"  "five"  "six"  "seven"  "eight"  "nine"  "zero"


-- the inputs of the function must belong to Eq TypeClass 
-- the function returns the index of the first letter of the word.

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromJust $ findIndex (isPrefixOf search) (tails str)

-- :: "two1nine" -> Digit -> "21nine"
replace :: (Show p, Enum p) => p -> [Char] -> [Char]
replace n str  =
  let
  pos = findString word str
  digit = show (fromEnum n)
  word = map toLower (show n)
  in take pos str ++ digit ++ drop (length word) str

-- :: "eighttwothree" -> "8twothree" -> "823"
-- :: "zoneight234" -> "z1ight234"
-- getCorrectedCalibrationValues :: [[Char]] -> [Int]
-- getCorrectedCalibrationValues  = findString $ replaceWordWithDigit
--
