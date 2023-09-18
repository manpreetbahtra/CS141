module TuplesAndLists where

import Prelude hiding (curry, uncurry, null)

--------------------------------------------------------------------------------
-- PAIRS

pair :: a -> b -> (a,b)
pair a b = (a,b)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

--------------------------------------------------------------------------------
-- TRIPLES

birthday :: (Int, Int, Int)
birthday = (13,12,2003)

today :: (Int,Int,Int)
today = (23,01,2023)

age :: (Int,Int,Int) -> (Int,Int,Int) -> Int
age (bd,bm,by) (td,tm,ty) = if tm > bm || (tm == bm && td >=bd) then ty-by else ty-by-1

--------------------------------------------------------------------------------
-- LISTS

oneTwoThree :: [Int]
oneTwoThree = [1,2,3]

null :: [a] -> Bool
null [] = True
null _ = False


isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = reverse a == a

sayTimes :: String -> Int -> String
sayTimes s x = concat(replicate x s) 