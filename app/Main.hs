module Main where

import qualified MyLib (getDigitsInEntries, f)

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ MyLib.getDigitsInEntries $ lines dt
  
