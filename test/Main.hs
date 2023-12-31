import Test.Tasty
import Test.Tasty.HUnit
import qualified MyLib (getCalibrationValues, getCorrectedCalibrationValues)

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
    testCase "task2: getCorrectedCalibrationValues" $
    MyLib.getCorrectedCalibrationValues testVals2 @?= [29, 83, 13, 24, 42, 14, 76]
    ]

main :: IO ()
main = defaultMain tests
