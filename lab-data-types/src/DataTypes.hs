module DataTypes where

--------------------------------------------------------------------------------
-- Exercise 1: Grade

data Grade = First | TwoOne | TwoTwo | Third | Fail 
  deriving (Eq,Show,Ord)


toGrade :: Int -> Grade
toGrade n 
  | n >= 70 = First
  | n >= 60 && n<=69 = TwoOne
  | n >= 50 && n<=59 = TwoTwo
  | n >= 40 && n<=49 = Third
  | otherwise = Fail


getBoundaries :: Grade -> (Int, Int)
getBoundaries x
  | x == First = (70,100)
  | x == TwoOne = (60,69)
  | x == TwoTwo = (50,59)
  | x == Third = (40,49)
  | otherwise = (0,39)


toGrade' :: Int -> Maybe Grade
toGrade' x = if x >=0 && x <=100
              then Just (toGrade x )
              else Nothing


--------------------------------------------------------------------------------
-- Exercise 2: Shapes

data Circle = Circle Float
  deriving (Eq,Show)

class Shape a where
  perimeter :: a -> Float
  area      :: a -> Float
  unit      :: a

instance Shape Circle where
  perimeter (Circle r) = 2 * pi * r
  area      (Circle r) = pi * r^2
  unit = Circle 1 



----------------------------------------
data Triangle = Triangle Float Float Float
  deriving (Eq,Show)

-- Put your Shape instance for Triangle here
instance Shape Triangle where
  perimeter (Triangle a b c ) = a + b + c
  area      (Triangle a b c) = 
    let 
      d = 0.5 * perimeter
    in
      sqrt (d * ((d-a) * ((d-b) * (d-c))))
  unit = Triangle 1 1 1 


----------------------------------------
data Quad = Square Float | Rectangle Float Float 
--Square and rectangle are called constructors
  deriving(Eq, Show)


-- Put your Shape instance for Quad here
instance Shape Quad where
  perimeter (Square a) = 4 * a
  perimeter (Rectangle a b ) = 2 * (a + b )
  area (Square a) = a * a
  area (Rectangle a b) = a * b 
  unit = Square (1) 