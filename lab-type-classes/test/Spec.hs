{-# LANGUAGE TypeApplications #-}

import Test.Tasty
    ( defaultMain, TestTree(..), TestTree(..), testGroup )
import Test.Tasty.HUnit
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.QuickCheck

import TypeClasses
import Control.Monad (replicateM)
import Control.Applicative

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
      testProperty "numberStringsTo - generates the correct strings"
      $ property
      $ forAllShrink (choose (1,1000)) shrinkIntegral
      $ \n -> numberStringsTo n === (show <$> [1..n])
    , testProperty "anyZero - checks if zero is in the list correctly"
      $ property
      $ forAllShrink (choose (1,5) >>= flip replicateM (choose @Integer (0,5)))
        genericShrink
      $ \ns -> (0 `elem` ns) === (anyZero . unwords . map show $ ns)
    , testGroup "plusMinus"
      [
        testProperty "works on integers"
        $ property
        $ forAllShrink (arbitrary :: Gen Integer) shrinkIntegral
        $ \n -> let (x,y) = plusMinus n in x <= 0 && y >= 0 && x + y == 0 && n `elem` [x,-x]
      , 
        testProperty "works on floats"
        $ property
        $ forAllShrink (arbitrary :: Gen Float) shrinkRealFrac
        $ \n -> let (x,y) = plusMinus n in x <= 0 && y >= 0 && x + y == 0 && n `elem` [x,-x]
      ]
    , testProperty "appears - behaves the same as `elem` from Prelude"
      $ property 
      $ forAllShrink (do 
                        NonEmpty xs <- arbitrary @(NonEmptyList Integer)
                        d <- arbitrary
                        y <- if d then elements xs else pure 5
                        pure (xs, y)) genericShrink
      $ \(xs,y) -> elem y xs === appears y xs
    , testProperty "largest3 - gives back the largest of the three elements"
        $ property
        $ forAllShrink ((,,) <$> arbitrary @Integer <*> arbitrary @Integer <*> arbitrary @Integer)     
            genericShrink
        $ \(a,b,c) -> largest3 a b c === maximum [a,b,c]
    , testGroup "eAnd, eOr, eNot"
      [
        testProperty "eAnd - behaves like (&&)"
          $ property
          $ \(x,y) -> eAnd x y === (x && y)
      , testProperty "eOr - behaves like (||)"
          $ property
          $ \(x,y) -> eOr x y === (x || y)
      , testProperty "eNot - behaves like not"
          $ property
          $ \x -> eNot x === not x
      ]
    , testCaseSteps "pog" $ \step -> do
        step "Sorry, there are no tests for Ex. 8 onwards."
        step "If you get stuck, contact a tutor."
    ]