module RPN where

import State

data Term = Plus | Minus | Times | Val Integer
  deriving (Eq, Show)

push :: Integer -> State [Integer] ()
push x = error "Not implemented"

pop :: State [Integer] Integer
pop = error "Not implemented"

evalTerm :: Term -> State [Integer] ()
evalTerm (Val v) = error "Not implemented"
evalTerm Plus    = error "Not implemented"
evalTerm Minus   = error "Not implemented"
evalTerm Times   = error "Not implemented"

evalRPN :: [Term] -> State [Integer] [()]
evalRPN = error "Not implemented"

runRPN :: [Term] -> Integer
runRPN = error "Not implemented"
