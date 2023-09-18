import Definitions

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Prelude hiding (not)

import Test.Tasty.QuickCheck ( testProperty, Testable(property), choose )

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Definitions"
    [ testNotIf, testNotCase, testNotTLP, testNotGuard, testFac, testFoo ]

testNotIf :: TestTree
testNotIf =
    testGroup "notIf (if...then...else...)"
    [ testCase "notIf True is False" $ notIf True  @?= False
    , testCase "notIf False is True" $ notIf False @?= True
    ]

testNotCase :: TestTree
testNotCase =
    testGroup "notCase (case...of...)"
    [ testCase "notCase True is False" $ notCase True  @?= False
    , testCase "notCase False is True" $ notCase False @?= True
    ]

testNotTLP :: TestTree
testNotTLP =
    testGroup "notTLP (top level pattern matching)"
    [ testCase "notTLP True is False" $ notTLP True  @?= False
    , testCase "notTLP False is True" $ notTLP False @?= True
    ]

testNotGuard :: TestTree
testNotGuard =
    testGroup "notGuard (guards)"
    [ testCase "notGuard True is False" $ notGuard True  @?= False
    , testCase "notGuard False is True" $ notGuard False @?= True
    ]

testFac :: TestTree
testFac = testGroup "fac"
    [ testProperty "generates the factorial of a number"
        $ property $ do
            n <- choose (0,1000)
            pure $ fac n == product [1..n]
    ]

testFoo :: TestTree
testFoo = testGroup "foo"
    [ testProperty "foo and fooLambda have the same functionality"
        $ property $ \x y -> foo x y == fooLambda x y
    , testProperty "foo and fooGolfed have the same functionality"
        $ property $ \x y -> foo x y == fooGolfed x y
    ]