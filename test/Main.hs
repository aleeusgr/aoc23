import Test.Tasty
import Test.Tasty.HUnit
import qualified MyLib (getCalibrationValues)

tests :: IO String -> TestTree
tests lsIO = testGroup "tests"
    [ testCase "task1: the sum of calibration numbers is correct" $ 
    sum ( MyLib.getCalibrationValues $ lines lsIO) @?= 56397 
    ]

main :: IO ()
main = defaultMain (withResource acquire tests)

acquire :: IO [String]
acquire = read <$> readFile "inputs/1"

