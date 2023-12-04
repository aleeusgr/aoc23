module Main where

--import qualified MyLib (someFunc)
import Data.Char

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ getDigitsInEntries $ lines dt
  
