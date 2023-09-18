import Test.Tasty ( defaultMain, TestTree(..) )
import Test.Tasty.HUnit

import Test.Tasty ( TestTree(..), testGroup )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck

import Data.UTree
import Trees
import qualified Data.List as List
import Data.Foldable (toList)

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    putStrLn "These tests rely on the Foldable instance, and will not give sensible results until after Ex. 11 is completed. \n"
    muffledMain tests

tests :: TestTree
tests = testGroup "UTrees" 
    [
      testCase "The empty UTree contains zero elements"
        $ null (empty :: UTree Int) @?= True
    , testProperty "The (singleton x) UTree contains only x"
        $ property $ \x -> toList (singleton x :: UTree Int) === [x]
    , testProperty "insert does not insert duplicate values"
        $ property 
        $ forAllShrink (arbitrary) shrinkIntegral $ \x -> 
          forAllShrink (listOf arbitrary) genericShrink $ \xs -> 
            length (fromList (x:x:xs) :: UTree Int) === length (fromList (x:xs))
    , testProperty "UTree elements are in sorted order"
        $ property 
        $ forAllShrink (listOf arbitrary) genericShrink $ \xs -> 
            let xs' = toList $ fromList xs :: [Int]
            in  and $ zipWith (<) xs' (tail xs') 
    , testProperty "UTree contains no duplicate elements"
        $ property 
        $ forAllShrink (listOf arbitrary) genericShrink $ \xs -> 
            let xs' = toList $ fromList xs :: [Int]
            in length (List.nub xs') === length xs'
    , testProperty "nubSort removes duplicates and sorts the list"
        $ property
        $ forAllShrink (listOf arbitrary) genericShrink $ \xs ->
            List.nub (List.sort xs :: [Int]) === nubSort xs
    ]