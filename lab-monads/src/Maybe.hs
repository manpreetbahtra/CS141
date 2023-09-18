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
  (>>=) = error "Not implemented"


--------------------------------------------------------------------------------
-- Working with Maybe


-- Return Nothing if y is not a divisor of x.
perfectDiv :: Integer -> Integer -> Maybe Integer
perfectDiv = error "Not implemented"


perfectHalf :: Integer -> Maybe Integer
perfectHalf x = error "Not implemented"


perfectSixteenth :: Integer -> Maybe Integer
perfectSixteenth x = error "Not implemented"


perfectSixteenth' :: Integer -> Maybe Integer
perfectSixteenth' = error "Not implemented"