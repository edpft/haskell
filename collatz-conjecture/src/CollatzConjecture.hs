module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = doCollatz n 0

doCollatz :: Integer -> Integer -> Maybe Integer
doCollatz n acc
  | n <= 0 = Nothing
  | n == 1 = Just acc
  | even n =
      doCollatz (div n 2) (acc + 1)
  | odd n = doCollatz (3 * n + 1) (acc + 1)