{-# LANGUAGE TypeApplications #-}
import TuplesAndLists

import Test.Tasty ( defaultMain, TestTree(..) )
import Test.Tasty.HUnit

import Test.Tasty ( TestTree(..), testGroup )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.QuickCheck

import Prelude hiding (curry, uncurry, null)
import qualified Prelude

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Tests"
    [testPair, testTriple, testNull, testPalindrome, testSayTimes]

testPair :: TestTree
testPair = testGroup "pairs" [
    testGroup "pair"
    [ testProperty "Gives back the elements paired together"
      $ property ((\x y -> pair x y == (x,y)) :: Int -> Int -> Bool)
    ]
    ,
    testGroup "swap"
    [ testProperty "Swaps the two elements over"
      $ property ((\x y -> swap (x,y) == (y,x)) :: Int -> Int -> Bool)
    ]
    ]

testTriple :: TestTree
testTriple = testGroup "triples" [
    testGroup "age" [
        testCase "Age is sensible" $ let (dd,mm,yyyy) = birthday
        in do
            assertBool "Day of the month is out of the range [1..31]"
                $ dd >= 1 && dd <= 31
            assertBool "Month is out of the range [1..12]"
                $ mm >= 1 && mm <= 12
            assertBool "Date is too far in the past"
                $ yyyy >= 1990
    ]
    , testGroup "today" [
        testCase "Today's date is sensible" $ let (dd,mm,yyyy) = today
        in do
            assertBool "Day of the month is out of the range [1..31]"
                $ dd >= 1 && dd <= 31
            assertBool "Month is out of the range [1..12]"
                $ mm >= 1 && mm <= 12
            assertBool "Date is too far in the past"
                $ (yyyy,mm,dd) >= (2022,1,15)
    ]
    , testGroup "age" [
        testProperty "Correctly computes age"
            $ property $ do
                bd <- choose @Int (1,31)
                bm <- choose @Int (1,12)
                by <- choose @Int (1900,2010)
                td <- choose @Int (1,31)
                tm <- choose @Int (1,12)
                ty <- choose @Int (2000, 3000)
                let after = (tm, td) >= (bm, bd)
                pure $
                    age (bd,bm,by) (td,tm,ty) ==
                    (if after then ty-by else ty-by-1)
        ,
        testCaseSteps "Computes your age" $ \step -> do
            let
                (d,m,y) = today
                (bd,bm,by) = birthday
                a = age birthday today
            step $ "Based on `birthday` and `today`, you are " ++ show a ++ " years old!"
    ]
    ]

testNull :: TestTree
testNull = testGroup "null"
    [ testCase "gives True for empty lists"
        $ null [] @?= True
    , testProperty "gives False for nonempty lists"
        $ property $ \x -> Prelude.null x || (not $ null @[Int] x) 
    ]

testPalindrome :: TestTree
testPalindrome = testGroup "palindrome"
    [ testCase "returns True  for \"racecar\""
        $ isPalindrome "racecar" @?= True
    , testCase "returns True  for \"tacocat\""
        $ isPalindrome "tacocat" @?= True
    , testCase "returns False for \"tacodog\""
        $ isPalindrome "tacodog" @?= False
    , testCase "returns False for \"()\""
        $ isPalindrome "()" @?= False
    , testCase "returns True  for \"))\""
        $ isPalindrome "))" @?= True
    ]

testSayTimes :: TestTree
testSayTimes = testGroup "sayTimes"
    [ testProperty "Repeats its input the right number of times"
        $ property $ \a -> length (sayTimes (a::String) 5) == 5 * length a
    ]