module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)

isPangram :: String -> Bool
isPangram text
  | length text < 26 = False
isPangram text =
  let lowerCaseLetters = toLowerCaseLetters text
      uniqueLowerCaseLetters = toUnique lowerCaseLetters
      letterCount = length uniqueLowerCaseLetters
   in letterCount == 26

toLowerCaseLetters :: String -> String
toLowerCaseLetters text = [toLower letter | letter <- text, isAscii letter, isAlpha letter]

toUnique :: (Eq a) => [a] -> [a]
toUnique [] = []
toUnique list = foldl appendUnique [] list

appendUnique :: (Eq a) => [a] -> a -> [a]
appendUnique [] element = [element]
appendUnique list element = if element `elem` list then list else list ++ [element]