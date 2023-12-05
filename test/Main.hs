import Test.Tasty
import Test.Tasty.HUnit
import qualified MyLib (getCalibrationValues)

-- TODO: make it work and have task one validate 56397
-- acquire :: IO [String]
-- acquire = read <$> readFile "inputs/1"

testVals :: [String]
testVals = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

tests :: TestTree
tests  = testGroup "tests"
    [ testCase "task1: the sum of calibration numbers is correct" $ 
    sum ( MyLib.getCalibrationValues testVals) @?= 142  
    ]

main :: IO ()
main = defaultMain tests


