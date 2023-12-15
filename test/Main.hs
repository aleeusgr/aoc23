import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified MyLib (getCalibrationValues, getCorrectedCalibrationValues, findAndReplaceNumber, correctCalibrationValues)

-- TODO: make it work and have task one validate 56397
-- acquire :: IO [String]
-- acquire = read <$> readFile "inputs/1"

testVals1 :: [String]
testVals1 = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

testVals2 :: [String]
testVals2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four",
             "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

digs = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tests :: TestTree
tests = testGroup "All" [unitTests, qcProps]

unitTests :: TestTree
unitTests  = testGroup "unit tests"
    [ testCase "task1: the sum of calibration numbers is correct" $ 
    sum ( MyLib.getCalibrationValues testVals1) @?= 142
    ,
    testCase "task2: getCorrectedCalibrationValues" $
    MyLib.getCorrectedCalibrationValues testVals2 @?= [29, 83, 13, 24, 42, 14, 76]
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [
      QC.testProperty "correctCalibrationValues returns ..." $
      \x -> length (MyLib.correctCalibrationValues x :: [Char] )  <= length x
      -- ,
      -- QC.testProperty "findAndReplaceNumber returns ..." $
      -- \x -> length (MyLib.findAndReplaceNumber (x :: [Char]) digs )  <= length x
  ]

main :: IO ()
main = defaultMain tests
