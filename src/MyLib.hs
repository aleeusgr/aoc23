module MyLib (getCalibrationValues, Digits (..), getCorrectedCalibrationValues, findAndReplaceFirstOccurrence) where

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

-- TODO:
-- dispDigits :: Digits -> Int
-- dispDigits x = toLower $ show x
-- this is cool, so what I misunderstand is list vs data, what is the relationship?

-- findString :: (Eq a) => [a] -> [a] -> Maybe Int
-- findString search str = findIndex (isPrefixOf search) (tails str)

testVals2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]
digs = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tv = testVals2 !! 1
dig = digs !! 8

-- this function parses once, but now I need to rerun it until I can say there are no words left in the string.
findAndReplaceNumber :: [Char] -> [String] -> [Char]
findAndReplaceNumber tv [] = head tv : findAndReplaceFirstOccurrence (tail tv) digs
findAndReplaceNumber tv digs =
    let dig = head digs
        l = length dig
     in if dig == take l tv
            then wordToDigit dig ++ drop l tv
            else findAndReplaceFirstOccurrence tv (tail digs)

getDigitsInWord tv = map (`isSubsequenceOf` tv) digs

findAndReplaceAll word =
    let digitList = getDigitsInWord word
     in if any True digitList
            then findAndReplaceNumber word digs
            else word

--   if length tv > 2 then
--   if string contains no words return string
--   else find and replace.
--   map isSubsequenceOf

-- getCorrectedCalibrationValues :: [[Char]] -> [Int]
getCorrectedCalibrationValues x = x

-- getCorrectedCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries (map replaceAll xs)
