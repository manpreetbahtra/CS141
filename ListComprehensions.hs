module ListComprehensions where

import Data.List

--------------------------------------------------------------------------------

-- Returns True iff the first argument can be divided by the second with no remainder.
dividesBy :: Integer -> Integer -> Bool
dividesBy x y = if x `mod` y == 0  then True else False


-- Whether or not the argument is prime.
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = if not (or ([dividesBy n x | x <- [2..floor(sqrt(fromIntegral n ))]])) then True else False


-- The infinite list of all prime numbers.
primes :: [Integer]
primes = [n | n <- [1..], isPrime n]


-- All possible outcomes of rolling two die, with number of sides m and n, including duplicates.
rollOutcomes :: Int -> Int -> [Int]
rollOutcomes m n = [m + n | m <- [1..m], n<- [1..n] ]


-- The Cartesian product of two lists.
cartProd :: [a] -> [b] -> [(a,b)]
cartProd m n= [(a,b) | a <- m, b <- n]


-- Given a function and two lists, apply f to every pair of elements from the two lists.
cartProdWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartProdWith f m n  = [f a b | a <- m, b <- n] 


-- Implement rollOutcomes again, using the cartProdWith function defined above.
rollOutcomes' :: Int -> Int -> [Int]
rollOutcomes' m n = cartProdWith (+) [1..m] [1..n] 


-- The full lowercase alphabet.
letters :: [Char]
letters = ['a'..'z']


-- The five lowercase vowels.
vowels :: [Char]
vowels =  ['a','e','i','o','u']


-- The twenty-one lowercase consonants.
consonants :: [Char]
consonants = letters \\ vowels


-- A function which gives back "FizzBuzz" if the number is a multiple of 3 and 5; "Fizz" if only a multiple of 3; "Buzz" if only a multiple of 5; and the number as a string otherwise.
fb :: Integer -> String
fb n = if n `dividesBy` 3 && n `dividesBy` 5 then "FizzBuzz" else (if n `dividesBy` 3 then "Fizz" else (if n `dividesBy` 5 then "Buzz" else show n ))

-- Using `map`, compute the full FizzBuzz from 1 up to the argument given.
fizzbuzzTo :: Integer -> [String]
fizzbuzzTo x= map fb[1..x]


-- How many times is "Fizz" printed between 1 and 1000?
howManyFizzes :: Int
howManyFizzes = length (filter (\a -> a == "Fizz" || a == "FizzBuzz" ) (fizzbuzzTo 1000) )--Length(filter fizzbuzzto 1000)