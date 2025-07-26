module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA dnaString = doToRna dnaString ""

doToRna :: String -> String -> Either Char String
doToRna [] rnaString = Right rnaString
doToRna (dna : dnaString) rnaString = case toRnaChar dna of
  Right rna -> doToRna dnaString (rnaString ++ [rna])
  Left rna -> Left rna

toRnaChar :: Char -> Either Char Char
toRnaChar letter
  | letter == 'G' = Right 'C'
  | letter == 'C' = Right 'G'
  | letter == 'T' = Right 'A'
  | letter == 'A' = Right 'U'
  | otherwise = Left letter
