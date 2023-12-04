module Main where

--import qualified MyLib (someFunc)
splitLines :: [Char] -> [String]
splitLines dt = 
  let
    contents = lines dt
  in
    contents

main :: IO ()
main = do
  dt <- readFile "inputs/1"
  print $ splitLines dt
  
