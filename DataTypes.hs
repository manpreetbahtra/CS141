module DataTypes where

--------------------------------------------------------------------------------
-- Exercise 1: Grade

data Grade = Fail | Third | TwoTwo | TwoOne | First
  deriving (Show, Eq, Ord)


toGrade :: Int -> Grade
toGrade x 
  |x >= 70 = First
  |x>= 60 && x <70 = TwoOne
  |x >=50 && x <60 = TwoTwo
  |x >=40 && x <50 = Third
  | otherwise  = Fail


getBoundaries :: Grade -> (Int, Int)
getBoundaries x 
 | x == First = (70,100)
 | x == TwoOne = (60, 69)
 | x == TwoTwo = (50,59)
 | x == Third = (40, 49)
 | otherwise = (0,39)


toGrade' :: Int -> Maybe Grade
toGrade' x 
  -- if x >=0 && x <=100 then Just toGrade x else Nothing
  | x >= 0 && x <=100 = Just $ toGrade x 
  |otherwise = Nothing


--------------------------------------------------------------------------------
-- Exercise 2: Shapes

data Circle = Circle Float
  deriving (Eq, Show)

class Shape a where
  perimeter :: a -> Float
  area      :: a -> Float
  unit      :: a

instance Shape Circle where
  perimeter (Circle r) = 2 * pi * r
  area      (Circle r) = pi * r * r 
  unit = Circle 1



----------------------------------------
data Triangle = Triangle Float Float Float
  deriving(Eq, Show)

-- Put your Shape instance for Triangle here
instance Shape Triangle where
  perimeter (Triangle s1 s2 s3) = s1 + s2 + s3
  area (Triangle s1 s2 s3) =
    let
      some = 0.5 * perimeter (Triangle s1 s2 s3) -- Use the perimeter function to calculate the perimeter
    in
      sqrt (some * (some - s1) * (some - s2) * (some - s3)) -- Calculate the area using Heron's formula
  unit = Triangle 1 1 1 



----------------------------------------
data Quad = Square Float | Rectangle Float Float
  deriving (Eq, Show)

-- Put your Shape instance for Quad here
instance Shape Quad where
  perimeter (Square s) = 4 * s
  perimeter (Rectangle s1 s2 ) = 2 * (s1 + s2)
  area (Square s ) = s * s
  area (Rectangle s1 s2) = s1 * s2
  unit = Square 1