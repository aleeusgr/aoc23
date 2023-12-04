module MyLib (getDigitsInEntries, f) where

import Data.Char

getDigitsInEntries :: [[Char]] -> [[Char]]
getDigitsInEntries = map (filter isDigit)

f :: [[a]] -> [a] 
f x = head x ++ last x
