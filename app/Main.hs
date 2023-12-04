module Main where

--import qualified MyLib (someFunc)
import Data.List.Split

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  contents <- lines dt 
  print contents
  
