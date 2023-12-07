import Test.Tasty
import Test.Tasty.HUnit
import Data.Char
import qualified MyLib (getCalibrationValues, Digits(..), replace, findString)

-- TODO: make it work and have task one validate 56397
-- acquire :: IO [String]
-- acquire = read <$> readFile "inputs/1"

testVals1 :: [String]
testVals1 = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

testVals2 :: [String]
testVals2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four",
             "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

tests :: TestTree
tests  = testGroup "tests"
    [ testCase "task1: the sum of calibration numbers is correct" $ 
    sum ( MyLib.getCalibrationValues testVals1) @?= 142
    ,
    testCase "task2: Digits can show digits" $
    fromEnum MyLib.Two @?= 2
    ,
    testCase "task2: Digits can show words" $
    map toLower (show MyLib.Two) @?= "two"
    ,
    testCase "task2: findString finds Two in head" $
    MyLib.findString (map toLower (show MyLib.Two)) (head testVals2) @?= 0
    ,
    testCase "task2: replace two with 2" $ 
    MyLib.replace MyLib.Two (testVals2 !! 1)  @?= "21nine"
    ,
    testCase "task2: replace two with 2 in eighttwothree" $
    MyLib.replace MyLib.Two (testVals2 !! 1)  @?= "eight2three"
    ,
    testCase "task2: replace nine with 9" $ 
    MyLib.replace MyLib.Nine (head testVals2)  @?= "two19"
    ,
    testCase "task2: getCorrectedCalibrationValues" $
    sum ( MyLib.getCalibrationValues testVals2) @?= 281
    ]

main :: IO ()
main = defaultMain tests
