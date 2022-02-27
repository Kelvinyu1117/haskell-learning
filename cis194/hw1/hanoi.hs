type Peg = String

type Move = (Peg, Peg)

-- Exercise 5
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi num_of_discs a b c
  | num_of_discs == 1 = [(a, b)]
  | otherwise = hanoi (num_of_discs - 1) a c b ++ [(a, b)] ++ hanoi (num_of_discs - 1) c b a