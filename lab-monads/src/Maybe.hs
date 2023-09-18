-- We use this extension so we can put the signatures in the instances :)
{-# LANGUAGE InstanceSigs #-}

module Maybe where

-- This line hide the default version of Maybe.
import Prelude hiding (Maybe(..))

--------------------------------------------------------------------------------
-- The Maybe data type

data Maybe a = Nothing | Just a
  deriving (Show, Eq)


instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just $ f x


instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _     = Nothing
  _ <*> Nothing     = Nothing
  Just f <*> Just x = Just $ f x


instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  -- implement the instance here!
  x >>= f = case x of
      Nothing -> Nothing
      Just q -> f q
  -- Nothing >>= f = Nothing
  -- Just x >>= f = f x 


--------------------------------------------------------------------------------
-- Working with Maybe


-- Return Nothing if y is not a divisor of x.
perfectDiv :: Integer -> Integer -> Maybe Integer
perfectDiv x y 
  | y == 0 = Nothing
  | x `mod` y == 0 = Just $ x `div` y
  | otherwise = Nothing
-- perfectDiv = error"Not implemenetd"


perfectHalf :: Integer -> Maybe Integer
perfectHalf x = perfectDiv x 2 


perfectSixteenth :: Integer -> Maybe Integer
perfectSixteenth x = 
  case perfectHalf x of 
    Nothing -> Nothing
    Just b -> case perfectHalf b of 
                Nothing -> Nothing
                Just c -> case perfectHalf c of 
                            Nothing -> Nothing
                            Just d -> case perfectHalf d of
                                        Nothing -> Nothing
                                        Just e -> Just e


perfectSixteenth' :: Integer -> Maybe Integer
perfectSixteenth' x = perfectHalf x >>= perfectHalf >>= perfectHalf >>= perfectHalf