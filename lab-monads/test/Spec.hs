{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, LambdaCase #-}
import Test.Tasty ( defaultMain, TestTree(..) )
import Test.Tasty.HUnit

import Test.Tasty ( TestTree(..), testGroup )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck

import Prelude hiding (Maybe(..))
import Maybe

import Control.Monad
import State
import RPN

import Data.List (scanl')


--------------------------------------------------------------------------------
-- QuickCheck instances for our custom Maybe type

instance Arbitrary1 Maybe where
  liftArbitrary arb = frequency [(1, return Nothing), (3, liftM Just arb)]

  liftShrink shr (Just x) = Nothing : [ Just x' | x' <- shr x ]
  liftShrink _   Nothing  = []

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance {-# OVERLAPS #-} Arbitrary [Term] where
  arbitrary = listOf
    (elements $ [Plus, Minus, Times, Plus, Minus, Times] 
      ++ map Val [0..10])
    `suchThat` (all (\(x,y) -> y>x) . tail . scanl' f (0,0))
    `suchThat` (not.null)
    where
      f (a,b) = \case
        Plus  -> (a+1,b)
        Minus -> (a+1, b)
        Times -> (a+1, b)
        Val _ -> (a, b+1)

  shrink = subterms

--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ testMaybe
    , testState
    , testRPN 
    ]

testMaybe :: TestTree 
testMaybe = testGroup "Part I: Maybe.hs"
  [
    testGroup "(>>=)"
      [
        testProperty "Nothing cases are handled correctly"
        $ \(Fn (f::Integer -> Maybe Integer)) -> 
              (Nothing >>= f) === Nothing
      , 
        testProperty "Just cases are handled correctly"
        $ \(Fn (f::Integer -> Maybe Integer), x :: Integer) -> 
              (Just x >>= f) === f x
      ]
    ,
    testGroup "perfectDiv"
      [
        testProperty "All perfect divisions return Just values"
        $ \(x :: Integer) (y :: Integer) -> let 
            z = x * y
            in z `perfectDiv` y == Just x
              || (y == 0 && z `perfectDiv` y == Nothing)
      , testProperty "All Nothing values are imperfect divisions"
      $ \(x::Integer) (y :: Integer) -> 
          case x `perfectDiv` y of
            Nothing -> y == 0 || x `rem` y /= 0
            Just x  -> True
      ]
    ,
    testProperty "perfectHalf works as expected"
      $ \(x :: Integer) -> perfectHalf x ==
          if even x 
            then Just $ x `div` 2
            else Nothing
    ,
    testProperty "perfectSixteenth works as expected"
      $ \(x :: Integer) -> perfectSixteenth x == 
          if x `mod` 16 == 0
            then Just $ x `div` 16
            else Nothing
    ,
    testProperty "perfectSixteenth' works as expected"
      $ \(x :: Integer) -> perfectSixteenth' x == 
          if x `mod` 16 == 0
            then Just $ x `div` 16
            else Nothing
  ]

testState :: TestTree 
testState = testGroup "Part II: State.hs"
  [
    testProperty "modify works as expected"
      $ \(Fn (f :: Integer -> Integer)) (x :: Integer) -> 
        snd (runState (modify f) x) === f x
  ,
    testProperty "evalState works as expected"
      $ \(x :: Integer) (y :: Integer) ->
        let s = put y >> pure x
        in x == evalState s 0
  , 
    testProperty "execState works as expected"
      $ \(x :: Integer) (y :: Integer) ->
        let s = put y >> pure x
        in y == execState s 0
  ]  

testRPN :: TestTree 
testRPN = testGroup "Part III: RPN.hs"
  [
    testProperty "push adds a value to the top of the stack"
      $ \(x :: Integer) (st :: [Integer]) ->
        execState (push x) st === (x:st)
  , 
    testProperty "pop removes a value from the top of the stack and returns it" 
      $ \(x :: Integer) (st :: [Integer]) ->
        let (x',st') = runState (push x *> pop) st
        in x' == x && st' == st
  , 
    testGroup "evalTerm"
      [
        testProperty "Val pushes values correctly"
          $ \(x :: Integer) (st :: [Integer]) ->
            execState (evalTerm (Val x)) st === x:st
      , 
        testProperty "Plus computes values correctly"
          $ \(x :: Integer) (y :: Integer) (st :: [Integer]) ->
            execState (evalTerm Plus) (x:y:st) === ((x+y):st)
      , 
        testProperty "Minus computes values correctly"
          $ \(x :: Integer) (y :: Integer) (st :: [Integer]) ->
            execState (evalTerm Minus) (x:y:st) === ((x-y):st)
      , 
        testProperty "Times computes values correctly"
          $ \(x :: Integer) (y :: Integer) (st :: [Integer]) ->
            execState (evalTerm Times) (x:y:st) === ((x*y):st)
      ]
  ,
    testProperty "evalRPN combines terms correctly"
      $ \(ts :: [Term]) -> 
          execState (evalRPN ts) [] === 
          execState (sequenceA [evalTerm t | t <- ts]) []
  , 
    testProperty "runRPN computes the value properly"
      $ \(ts :: [Term]) -> 
          runRPN ts === (head . (`execState` []) . evalRPN) ts
  ]