{-
Contains warmup with GHCi as well as homework set 1. Some of the
homework problems come from 'Programming in Haskell' by Graham
Hutton, Chapters 1 and 2
-}

-- load this file in GHCi and you will be able to evaluate 'double' and
-- 'quadrapule' by plugging in x-values, i.e.  'double 4', 'quadruple 4'
-- which will return 8 and 16, respectively
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
-- x `f` y means f x y
average ns = sum ns `div` length ns

-- quicksort in haskell
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x ]
    larger = [b | b <- xs, b > x]

revqsort [] = []
revqsort (x:xs) = revqsort larger ++ [x] ++ revqsort smaller
  where
    smaller = [a | a <- xs, a <= x ]
    larger = [b | b <- xs, b > x]

{-
Q: What would be the effect of replacing <= by < in the definition of qsort?
A1: Duplicate elements are removed from the sorted list
A2: The sorting function will only work correctly on some inputs
-}

foo = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- alternative implementation of built-in function 'last'
{-
last' [] = []
last' (x:xs) =
  if length xs == 1
    then xs
  else tail xs
-}
-- the above fails on [1,2,3,4,5], returning [3,4,5]
-- it also fails on [1] or single-element lists
-- Q: how can I recursively apply 'tail' continuously
-- against 'xs' until 'xs' is []?

mylast (x:xs) = head (reverse xs)

{-
-- other possible implementations of last are as follows:

last' xs = head (drop (length xs - 1) xs)
last' xs = xs !! (length xs -1)

-}

-- define init s.t. it returns a list with the last element removed
init xs = reverse (tail (reverse xs))

-- Define a function product' that produces the product of a list of numbers,
-- and show using your definition that product [2, 3, 4] = 24.

{-
product' [] = 1
product' (x:xs) = x * product' xs

product' [2, 3, 4]
=   { applying product' }
2 ∗ (product' [3,4])
=   { applying product' }
2 ∗ (3 ∗ product' [4])
=   { applying product' }
2 ∗ (3 ∗ (4 ∗ product' [ ]))
=   { applying product' }
2 ∗ (3 ∗ (4 ∗ 1))
=   { applying ∗ }
24
-}
