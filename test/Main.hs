import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified MyLib (findAndReplaceNumber, getCalibrationValues, getCorrectedCalibrationValues)

-- TODO: make it work and have task one validate 56397
-- acquire :: IO [String]
-- acquire = read <$> readFile "inputs/1"

testVals1 :: [String]
testVals1 = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

testVals2 :: [String]
testVals2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four",
             "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

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

qcProps = testGroup "(checked by QuickCheck)"
  [  QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

main :: IO ()
main = defaultMain tests
