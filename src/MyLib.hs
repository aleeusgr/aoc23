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


data Digits = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Enum)
-- TODO:
-- dispDigits :: Digits -> Int
-- dispDigits x = toLower $ show x
-- this is cool, so what I misunderstand is list vs data, what is the relationship?

-- findString :: (Eq a) => [a] -> [a] -> Maybe Int
-- findString search str = findIndex (isPrefixOf search) (tails str)

testVals2 = ["two1nine" , "eightwothree" , "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]
digs = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

dt = testVals2 !! 1
dig = digs !! 8

-- isSubsequenceOf does not work because it finds two before eight in eightwothree 
-- my function must be more simple

-- compare str dn = 


--
-- :: "t" -> "z" -> False
-- :: "t" -> "o" -> False
-- tv3
-- :: "a" -> "z" -> False
-- :: "a" -> "o" -> False
--
-- :: "a" -> "t" -> False
--


-- bug:
-- test 1
-- :: "z" -> "t" -> False
-- :: "o" -> "t" -> False
-- :: "t" -> "t" -> True

-- test 2
-- :: "z" -> "e" -> False
-- :: "o" -> "e" -> False
-- :: "t" -> "e" -> True
-- so the input type is wrong, 



-- replaceAll :: [Char] -> [Char]
-- replaceAll str =
--   let
--     -- look at position 0
--     -- compareWith (0, Zero) (0, toLower $ show One) ..
--     -- if the name of the number 
--     -- then replace
--     -- else 
--     -- recurse on tail
--     --
--     -- at top level, how would I describe this operation?
--     -- I need to find numbers written as words and replace them with digit symbols
-- 
--     "eightwothree"
-- 
-- 
--   in str

-- getCorrectedCalibrationValues :: [[Char]] -> [Int]
getCorrectedCalibrationValues x = x
-- getCorrectedCalibrationValues xs = intList $ map convertToCalibrationValues $ getDigitsInEntries (map replaceAll xs)
