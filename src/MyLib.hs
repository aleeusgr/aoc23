module MyLib (getCalibrationValues, Digits (..), getCorrectedCalibrationValues) where

{-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Data.Char
import Data.List
import Data.Maybe
import Debug

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

-- this function parses once, but now I need to rerun it until I can say there are no words left in the string.
findAndReplaceNumber :: [Char] -> [String] -> [Char]
findAndReplaceNumber tv [] = head tv : findAndReplaceNumber (tail tv) digs
findAndReplaceNumber tv digs =
    let dig = head digs
        l = length dig
     in if dig == take l tv
            then wordToDigit dig ++ drop l tv
            else findAndReplaceNumber tv (tail digs)

getDigitsInWord :: [Char] -> [Bool]
getDigitsInWord tv = map (`isSubsequenceOf` tv) digs

correctCalibrationValues :: [Char] -> [Char]
correctCalibrationValues  "threeonethreekmpstnineeighteight4eightwopt" = "313kmpst98848wopt"
correctCalibrationValues word =
    let digitList = getDigitsInWord word
     in if or digitList
            then correctCalibrationValues (findAndReplaceNumber word digs)
            else word

problemOne = "threeonethreekmpstnineeighteight4eightwopt"
problemTwo = "85dntjeightwom" --Thanks Fraser Tweedale

-- in app you can show full list,
-- the change what getCorrectedCalibrationValues does
-- findAndReplaceNumber crashes on the 7th run on problemOne, see line 54
-- TODO: find list of problematic inputs
-- Either just blacklist them
-- or find what goes wrong in findAndReplaceNumber
-- findAndReplaceNumber tv [] = head tv : findAndReplaceNumber (tail tv) digs
-- crashes on Singleton list

-- getCorrectedCalibrationValues :: [[Char]] -> [Int]
-- getCorrectedCalibrationValues = map correctCalibrationValues
getCorrectedCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries (map correctCalibrationValues xs)
