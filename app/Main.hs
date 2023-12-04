module Main where

import qualified MyLib (getDigitsInEntries, dropMiddleDigits)

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ map MyLib.dropMiddleDigits $ MyLib.getDigitsInEntries $ lines dt
  
