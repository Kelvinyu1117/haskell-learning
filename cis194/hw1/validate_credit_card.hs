-- Exercise 1

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [mod n 10]

toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (n `div` 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther arr
  | null arr = arr
  | even (length arr) = head arr * 2 : doubleEveryOther (tail arr)
  | otherwise = head arr : doubleEveryOther (tail arr)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits arr
  | null arr = 0
  | length arr == 1 && (head arr `div` 10) == 0 = head arr
  | otherwise = sumDigits (toDigits (head arr)) + sumDigits (tail arr)

-- Exercise 4
validate :: Integer -> Bool
validate credit_card_num = mod (sumDigits (doubleEveryOther (toDigits credit_card_num))) 10 == 0