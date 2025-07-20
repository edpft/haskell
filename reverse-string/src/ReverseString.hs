module ReverseString (reverseString) where

reverseString :: String -> String
reverseString [] = []
reverseString [x] = [x]
reverseString (x : xs) = reverseString xs ++ [x]
