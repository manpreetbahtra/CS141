module Definitions where

{-
NOTE FROM ALEX:

The comments in lab sheets are just helpers to remind you what you have done.

You should still download and read the lab sheet - it has other things
that you need to know, and more exercises to complete!

Have fun :)
-}

--------------------------------------------------------------------------------

-- Implement `not` using the four different syntaxes we have seen.
{- I think top level pattern matching is the most appropriate syntactic sugar 
for not function because it is clear to understand. The least appropriate is guards 
since it is the least clear and could be difficult to grasp at first. 
-}


notIf, notCase, notTLP, notGuard :: Bool -> Bool

-- if...then...else...
notIf x = if x == True then False else True

-- case...of...
notCase x = case x of 
                True -> False
                False -> True

-- top level pattern matching
notTLP x = if x == True
                then False
                else True

-- guards
notGuard x 
    | x == True = False
    | x == False = True


--------------------------------------------------------------------------------
-- Implement the factorial function.

fac :: Integer -> Integer
fac n = if n == 0 then 1 else n * fac(n-1)


--------------------------------------------------------------------------------

-- Here is a weird function called interesting:
-- (p->q) acts as a function. p acts as an input. q is the output. So a is the function which is 
--applied to b. 
interesting :: (p -> q) -> p -> q
interesting a b = a b

-- Write interesting as a lambda function.
interestingLambda :: (p -> q) -> p -> q
interestingLambda = \a -> \b -> a b 

-- Write interesting as an operator called £.
-- £ acts as a parentheses. It says apply whatever is to the left of the £ to whatever is to the right of the £.
-- e.g. fac £ succ 2. So succ 2 acts as a input to fac. 
infixr 0 £
(£) :: (p -> q) -> p -> q
(£) a b = a b


--------------------------------------------------------------------------------

-- Here is a function called foo:
foo :: Int -> Int -> Int
foo x y = if x > y then x - y else y - x

-- Write foo as a lambda function.
fooLambda :: Int -> Int -> Int
fooLambda = \x -> \y -> if x > y then x - y else y - x

-- Golf the definition of foo.
fooGolfed :: Int -> Int -> Int
fooGolfed x y = abs £ x - y 