-- Exercise 1

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [mod n 10]

toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (n `div` 10)