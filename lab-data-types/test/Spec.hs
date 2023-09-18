import Test.Tasty ( defaultMain, TestTree(..) )
import Test.Tasty.HUnit

import Test.Tasty ( TestTree(..), testGroup )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit ( testCase, (@?=) )

import DataTypes
import Data.Foldable (asum, find)
import Data.Maybe (fromJust)
import Test.Tasty.QuickCheck


main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Tests"
    [testGrade, testShape]

testGrade :: TestTree
testGrade =
  testGroup "Grade"
  [
    testCase "Has all five constructors"
      $ length [Fail, Third, TwoTwo, TwoOne, First] @?= 5
    , testCase "Has a derived `Show` instance"
      $ show Fail @?= "Fail"
    , testCaseSteps "Uses the derived `Ord` instance properly"
      $ \step -> do
        assertBool "Fail < Third" $ Fail < Third
        assertBool "Third < TwoTwo" $ Third < TwoTwo
        assertBool "TwoTwo < TwoOne" $ TwoTwo < TwoOne
        assertBool "TwoOne < First" $ TwoOne < First
    , testProperty "toGrade gives the right return values"
      $ property $ forAll (choose (-10, 110))
      $ \x -> g x === toGrade x
    , testProperty "getBoundaries agrees with toGrade"
      $ property $ forAll (choose (0, 100))
      $ \x -> let (l,h) = getBoundaries $ toGrade x
              in x >= l && x <= h
    , testGroup "toGrade'"
      [ testProperty "When out of range - gives Nothing"
        $ property $ forAll (arbitrary `suchThat` (\x -> x < 0 || x > 100))
        $ \x -> toGrade' x === Nothing
      , testProperty "When in range - gives Just (the same answer as toGrade)"
        $ property $ forAll (choose (0,100))
        $ \x -> toGrade' x === Just (toGrade x)
      ]
  ]
  where
    allGrades :: [Grade]
    allGrades = [Fail, Third, TwoTwo, TwoOne, First]

    g :: Int -> Grade
    g x = snd $ fromJust
        $ find ((x<).fst)
        $ zip [40, 50, 60, 70, maxBound] allGrades

testShape :: TestTree
testShape =
  testGroup "Shapes"
  [
  testGroup "Circle"
    [
      testCase "Circle constructor exists" $
        case Circle 1 of {Circle x -> True} @?= True
    , testCase "Has a derived Show instance" $
        show (Circle 1) @?= "Circle 1.0"
    , testCase "Has a derived Eq instance" $
        Circle 1 == Circle 1 @?= True
    , testCase "The unit circle has perimeter 2pi" $
        perimeter (unit :: Circle) @?= 2 * pi
    , testCase "The unit circle has area pi" $
        area (unit :: Circle) @?= pi
    ]
  , testGroup "Triangle"
    [
      testCase "Triangle constructor exists" $
        seq Triangle True @?= True
    , testCase "Has a derived Show instance" $
        show (Triangle 1 1 1) @?= "Triangle 1.0 1.0 1.0"
    , testCase "Has a derived Eq instance" $
        Triangle 1 1 1 == Triangle 1 1 1 @?= True
    , testCase "The unit triangle has perimeter 3" $
        perimeter (unit :: Triangle) @?= 3
    , testCase "The unit triangle has area √3 / 4" $
        area (unit :: Triangle) @?= 0.25 * sqrt 3
    ]
  , testGroup "Quad"
    [
      testCase "Square and Rectangle constructors exist" $
        seq (Square 1, Rectangle 1 1) True @?= True
    , testCaseSteps "Has a derived Show instance" $
      \step -> do
        assertBool "...for Square" 
          $ show (Square 1) == "Square 1.0"
        assertBool "...for Rectangle" 
          $ show (Rectangle 1 1) == "Rectangle 1.0 1.0"
    , testCase "Has a derived Eq instance" $
        Square 1 == Square 1 @?= True
    , testCase "The unit square has perimeter 4" $
        perimeter (unit :: Quad) @?= 4
    , testCase "The unit square has area 1" $
        area (unit :: Quad) @?= 1
    ]
  ]