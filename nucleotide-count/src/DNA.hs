module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, fromList, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts dnaString = doNucleotideCounts dnaString (fromList [(A, 0), (C, 0), (G, 0), (T, 0)])

doNucleotideCounts :: String -> Map Nucleotide Int -> Either String (Map Nucleotide Int)
doNucleotideCounts [] counts = Right counts
doNucleotideCounts (dna : dnaString) counts = case countNucleotide dna counts of
  Right newNucleotideCounts -> doNucleotideCounts dnaString newNucleotideCounts
  Left _ -> Left dnaString

countNucleotide :: Char -> Map Nucleotide Int -> Either Char (Map Nucleotide Int)
countNucleotide character counts
  | character == 'A' = Right (insertWith (+) A 1 counts)
  | character == 'C' = Right (insertWith (+) C 1 counts)
  | character == 'G' = Right (insertWith (+) G 1 counts)
  | character == 'T' = Right (insertWith (+) T 1 counts)
  | otherwise = Left character
