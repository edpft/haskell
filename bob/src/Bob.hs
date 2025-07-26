module Bob (responseFor) where

import Data.Char
import Data.List

responseFor :: String -> String
responseFor xs
  | isShoutingQuestion trimmedString = "Calm down, I know what I'm doing!"
  | isQuestion trimmedString = "Sure."
  | isShouting trimmedString = "Whoa, chill out!"
  | isSilence trimmedString = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    trimmedString = trim xs

trim :: String -> String
trim [] = []
trim text = dropWhile isSpace (dropWhileEnd isSpace text)

isShoutingQuestion :: String -> Bool
isShoutingQuestion [] = False
isShoutingQuestion text = isShouting text && isQuestion text

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion text =
  let lastChar = last text
   in lastChar == '?'

isShouting :: String -> Bool
isShouting [] = False
isShouting text = any isUpper text && all isShoutyChar text

isShoutyChar :: Char -> Bool
isShoutyChar = not . isLower

isSilence :: String -> Bool
isSilence [] = True
isSilence text = all isSpace text