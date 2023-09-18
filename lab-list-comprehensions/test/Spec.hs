{-# LANGUAGE TypeApplications, LambdaCase #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.QuickCheck

import Data.Function (on)
import Data.List (sort)
import Data.Bool (bool)

import qualified Data.Numbers.Primes as P

import ListComprehensions

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testProperty "dividesBy - is implemented correctly"
      $ property
      $ forAll ((,) <$> choose (1,100) <*> choose (1,100))
      $ \(x,y) -> x `dividesBy` y === ((x `div` y) * y == x)
  , testProperty "isPrime - is True for primes and False otherwise"
      $ property
      $ forAllShrink (choose (1,100)) shrinkIntegral
      $ \x -> isPrime x === P.isPrime x
  , testProperty "primes - constructs the infinite list of primes"
      $ property
      $ forAllShrink (choose (1,1000)) shrinkIntegral
      $ \n -> take n primes === take n P.primes
  , testProperty "rollOutcomes - produces the right outcomes"
      $ property
      $ forAllShrink ((,) <$> choose (1,300) <*> choose (1,300)) genericShrink
      $ \(m,n) -> ((===) `on` sort)
          (rollOutcomes m n)
          ((+) <$> [1..m] <*> [1..n])
  , testProperty "cartProd - produces all pairs"
      $ property
      $ forAllShrink ((,) <$> choose (1,300) <*> choose (1,300)) genericShrink
      $ \(m,n) -> ((===) `on` sort)
          (cartProd @Integer @Integer [1..m] [1..n])
          ((,) <$> [1..m] <*> [1..n])
  , testProperty "cartProdWith - applies the function correctly"
      $ property
      $ forAllShrink ((,) <$> choose (1,300) <*> choose (1,300)) genericShrink
      $ \(m,n) -> ((===) `on` sort)
          (cartProdWith @Integer @Integer (,) [1..m] [1..n])
          ((,) <$> [1..m] <*> [1..n])
  , testProperty "rollOutcomes' - produces the same output as rollOutcomes"
      $ property
      $ forAllShrink ((,) <$> choose (1,300) <*> choose (1,300)) genericShrink
      $ \(m,n) -> ((===) `on` sort)
          (rollOutcomes' m n)
          (rollOutcomes m n)
  , testCase "letters - is \"abcdefghijklmnopqrstuvwxyz\""
      $ letters @?= "abcdefghijklmnopqrstuvwxyz"
  , testCase "vowels - is \"aeiou\""
      $ vowels @?= "aeiou"
  , testCase "consonants - is \"abcdfghjklmnpqrstvwxyz\""
      $ consonants @?= "bcdfghjklmnpqrstvwxyz"
  , testGroup "fb"
      [ testProperty "always prints \"Buzz\" or \"FizzBuzz\" for multiples of 5"
          $ forAllShrink ((*5) <$> choose (1,1000)) shrinkIntegral
          $ \n -> fb n `elem` ["Buzz", "FizzBuzz"]
      , testProperty "always prints \"Fizz\" or \"FizzBuzz\" for multiples of 3"
        $ forAllShrink ((*3) <$> choose (1,1000)) shrinkIntegral
        $ \n -> fb n `elem` ["Fizz", "FizzBuzz"]
      , testProperty "always prints the number if it's not a multiple of 3 or 5"
        $ forAllShrink (arbitrary `suchThat` (\n -> n `rem` 5 > 0 && n `rem` 3 > 0)) shrinkIntegral
        $ \n -> fb n === show n
      ]
  , testProperty "fizzbuzzTo - generates the correct list"
      $ property
      $ forAllShrink (choose (1,1000)) shrinkIntegral
      $ \n ->
          -- This is the worst fizzbuzz implementation ever written.
          -- Learn nothing from it.
          let mif = bool id
              fbs = [ \case{""->show x;x->x}
                    . mif (++"Buzz") (x`rem`5==0)
                    . mif (++"Fizz") (x`rem`3==0)
                    $ ""
                    | x <- [1..]
                    ]
          in fizzbuzzTo n === take (fromIntegral n) fbs

  ]
