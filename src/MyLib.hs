module MyLib (getCalibrationValues, Digits(..) ) where

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

-- data TestVals2 = {"two1nine"  "eightwothree"  "abcone2threexyz"  "xtwone3four"  "4nineeightseven2"  "zoneight234"  "7pqrstsixteen"}
--
data Digits = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving (Show, Enum)
-- "one" "two"  "three"  "four"  "five"  "six"  "seven"  "eight"  "nine"  "zero"


-- the inputs of the function must belong to Eq TypeClass 
-- the function returns the index of the first letter of the word.

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

-- get the length of the word
-- wordLength :: [Char] -> Int
-- wordLength = 


-- getNumber :: digitWord -> [Char]
-- getNumber = 

-- take part of the String from the beginning to the output of the findString and
-- add the number symbol
-- take the part from the beginning + length of the word
-- add to the result

-- :: "eighttwothree" -> "8twothree" -> "823"
-- :: "zoneight234" -> "z1ight234"
-- getCorrectedCalibrationValues :: [[Char]] -> [Int]
-- getCorrectedCalibrationValues  = findString $ replaceWordWithDigit
--
