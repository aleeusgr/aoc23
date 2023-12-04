module MyLib (getDigitsInEntries, dropMiddleDigits) where

import Data.Char

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

dropMiddleDigits :: [Char] -> [Char] 
dropMiddleDigits x = 
  let 
    h = [head x]
    l = [last x]
  in 
    h ++ l
