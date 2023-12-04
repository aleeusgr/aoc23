module Main where

--import qualified MyLib (someFunc)
task1 :: [Char] -> [String]
task1 dt = 
  let
    contents = lines dt
  in
    contents

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ task1 dt
  
