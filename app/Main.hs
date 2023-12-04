module Main where

--import qualified MyLib (someFunc)
import Data.Char

splitLines :: [Char] -> [String]
splitLines dt = 
  let
    contents = lines dt
  in
    contents

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)
  

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ getDigitsInEntries $ splitLines dt
  
