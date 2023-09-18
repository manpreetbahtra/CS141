{-# LANGUAGE InstanceSigs #-}

module State (State, runState, modify, evalState, execState, get, put) 
  where

--------------------------------------------------------------------------------
-- The State monad

-- The State data type wraps functions which return a value of type a and
-- may make use of (read and/or output) a value of type s.
-- Such functions are referred to as "stateful computations".
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  -- Mapping a function over a stateful computation means modifying
  -- its return value (the first component of function's return value)
  fmap :: (a -> b) -> State s a -> State s b
  fmap g sf = State $ gFirst . runState sf
    where gFirst (a,b) = (g a, b)

instance Applicative (State s) where
  -- A stateful computation that returns x and does nothing else.
  pure :: a -> State s a
  pure x = State { runState = \s -> (x, s) }

  -- Given a computation returning a function f,
  -- and a computation returning a value x,
  -- build a computation which runs both (in sequence). 
  -- manually threading through the stateful value ("s").
  -- The final output is a computation whose result is f x.
  (<*>) :: State s (a -> b) -> State s a -> State s b
  sf <*> sx = State $ \s -> 
    let 
      (f, s')  = runState sf s
      (x, s'') = runState sx s'
    in (f x, s'')

instance Monad (State s) where
  -- Given a stateful computation returning a value of type a,
  -- and a function which takes such an a and returning another stateful
  -- computation of type b, build a computation which runs the second function 
  -- with the result of the first, and threads the updated state through the
  -- result, generating a final value and a final updated stateful component.
  (>>=) :: State s a -> (a -> State s b) -> State s b
  sx >>= f = State $ \s -> 
    let
      (x, s') = runState sx s
      sy = f x
    in runState sy s'


-- Get the stateful value that was passed in and return it.
get :: State s s
get = State $ \s -> (s,s)

-- Set a new stateful value, and return () as the main value.
put :: s -> State s ()
put x = State $ \s -> ((),x) -- notice that s is unused!


--------------------------------------------------------------------------------


-- Get the stateful component, and pass it into a function which calls "put" to 
-- replace it with an updated version.
modify :: (s -> s) -> State s ()
modify f = error "Not implemented"


-- Given a stateful action and an initial state, evaluate the computation and
-- return the final value, discarding the final state.
evalState :: State s a -> s -> a
evalState s x = error "Not implemented"


-- Given a stateful action and an initial state, evaluate the computation and
-- return the final state, discarding the final value.
execState :: State s a -> s -> s
execState s x = error "Not implemented"