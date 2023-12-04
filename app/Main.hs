module Main where

--import qualified MyLib (someFunc)
import Data.Char

splitLines :: [Char] -> [String]
splitLines dt = 
  let
    contents = lines dt
  in
    contents

getDigitsInElement :: [[Char]] -> [[Char]]
getDigitsInElement = map (filter isDigit)
  

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ getDigitsInElement $ splitLines dt
  
