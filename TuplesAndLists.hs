module TuplesAndLists where

import Prelude hiding (curry, uncurry, null)

--------------------------------------------------------------------------------
-- PAIRS

pair :: a ->  b -> (a, b) 
pair a b = (a, b)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)  

--------------------------------------------------------------------------------
-- TRIPLES

birthday :: (Int, Int, Int)
birthday = (13,12,2003)

today :: (Int,Int,Int)
today = (21,01,2023)

age :: (Int,Int,Int) -> (Int,Int,Int) -> Int
age (bd,bm,by) (td,tm,ty) = error "Not implemented"

--------------------------------------------------------------------------------
-- LISTS

oneTwoThree :: [Int]
oneTwoThree = error "Not implemented"

null :: [a] -> Bool
null = error "Not implemented"

isPalindrome :: Eq a => [a] -> Bool
isPalindrome = error "Not implemented"

sayTimes :: String -> Int -> String
sayTimes = error "Not implemented"