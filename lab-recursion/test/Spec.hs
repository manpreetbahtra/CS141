{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

import Test.Tasty ( TestTree(..), testGroup, defaultMain )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Recursion

import Control.Applicative (liftA2)

import Prelude hiding (or, zip, zipWith)
import qualified Prelude as P
import qualified Data.Maybe as M
import Data.List ((\\), partition)

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [ testProperty "or behaves like or in Prelude"
        $ property $ liftA2 (===) or  P.or 
    , testProperty "or' behaves like or in Prelude"
        $ property $ liftA2 (===) or' P.or 
    , testProperty "catMaybes behaves like the catMaybes in Data.Maybe"
        $ property $ liftA2 (===) (catMaybes  @Integer) M.catMaybes
    , testProperty "catMaybes' behaves like the catMaybes in Data.Maybe"
        $ property $ liftA2 (===) (catMaybes' @Integer) M.catMaybes
    , testProperty "minima always returns the minimum"
        $ property $ \(xs :: [(Integer, Integer)]) -> and 
        $ P.zipWith (\a (b,c) -> a == (if b < c then b else c)) (minima xs) xs 
    , testProperty "sumToSeven keeps exactly those pairs which sum to 7"
        $ property $ forAllShrink (listOf $ (,) <$> choose @Integer (0,10) <*> choose (0,10)) genericShrink
        $ \xs -> let
          (as, bs) = partition (\(a,b) -> a+b==7) xs
          ss = sumToSeven xs
          in as == ss && bs == xs \\ ss
    , testProperty "zip behaves like zip in Prelude"
        $ property $ \xs ys -> zip @Integer @Char xs ys === P.zip xs ys
    , testProperty "zipWith behaves like zipWith in Prelude"
        $ property $ \xs ys -> zipWith ((+) @Integer) xs ys === P.zipWith (+) xs ys
    ]
