module TypeClasses where

-- | Generate the string representation of all numbers up to the argument.
numberStringsTo :: Int -> [String]
numberStringsTo x = map show [1..x]


-- | Returns True if there is a zero in the incoming list of space-separated 
-- integers.
anyZero :: String -> Bool
anyZero a = elem 0 (map read (words a))--filter [words "" read == 0]


-- | For a given number, returns the positive and negative versions of the 
-- number.
plusMinus :: (Ord a, Num a) => a -> (a,a)
plusMinus a 
    | a > 0 = (negate a, a)
    | a < 0 = (a, negate a )
    | otherwise = (0,0)


-- | Returns True if and only if the element appears in the list.
appears :: (Ord a, Eq a) => a -> [a] -> Bool
appears a b   -- filter `==` a if null then False else 
    | elem a b = True
    | otherwise = False


-- | Gets the largest of the 3 numbers, using (<=) and related functions.
largest3 :: (Num a, Ord a, Eq a ) => a -> a -> a -> a
largest3 x y z = if x >= y &&  x >= z then x else (if y >= x && y >= z then y else z)


-- | Computes the AND of two boolean values via integer maths.
eAnd :: Bool -> Bool -> Bool
eAnd x y = if x && y then True else False


-- | Computes the OR of two boolean values via integer maths.
eOr :: Bool -> Bool -> Bool
eOr x y = if x || y then True else False


-- | Computes the NOT of a boolean value via integer maths.
eNot :: Bool -> Bool 
eNot x  = not x

class Pog a where
    morePog :: a -> a -> Bool
    mostPog :: a

instance Pog Bool where
    morePog x y = False
    mostPog = True

instance Pog Integer where
    morePog x y = False
    mostPog = True

instance (Pog a, Pog b) => Pog (a,b) where
    morePog (a,b) (x,y) = if a `morePog` x then (a,b) else (if x `morePog` a then (x,y) else (if b `morePog` y then (a,b) else (x,y)))
    mostPog = (mostPog, mostPog)
