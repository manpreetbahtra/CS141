{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Recursion where
  
import Prelude hiding (or, zip, zipWith)

--------------------------------------------------------------------------------

-- | Takes a list of boolean values and returns True if any of them is True.
or :: [Bool] -> Bool
{- or [] = False
or (x:xs) 
    | True == x = True
    | otherwise = elem True xs  -}
{-another way of or -}

or [] = False
or (x:xs) = x || or xs
 
-- | or implemented using implicit recursion (the `any` function)
or' :: [Bool] -> Bool
or' [] = False
or' xs = any (==True) xs


--------------------------------------------------------------------------------

-- | Takes a list of Maybe values and collects together all the Just ones.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:xs) = x : catMaybes xs 
catMaybes (Nothing :xs)= catMaybes xs 

-- | catMaybes implemented using a fold.
catMaybes' :: [Maybe a] -> [a]
catMaybes' xs = foldr f [] xs
    where
        f :: (Maybe a ) -> [a] -> [a]
        f (Just x) xs = x : xs 
        f Nothing xs = xs


--------------------------------------------------------------------------------

-- | Given a list of pairs, returns the lower element for each pair.
minima :: Ord a => [(a,a)] -> [a]
{- minima [] = []
minima (x,y) = map min (uncurry (x,y))  
what i did above vs what i copied below-}

minima = map (uncurry min)
--------------------------------------------------------------------------------

-- | Keeps only the pairs of the input whose values sum to 7.
sumToSeven :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
sumToSeven xs = filter f xs
    where 
        f (x,y) = x + y == 7 


--------------------------------------------------------------------------------

-- | Pairs up the elements of two lists elementwise, until either list runs out.
zip :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip xs [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys


-- | Pairs up the elements of two lists elementwise, until either list runs out.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] ys = []
zipWith f xs [] = []
zipWith f (x:xs) (y:ys)= f x y : zipWith f xs ys 