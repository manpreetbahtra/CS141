{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Main where

import Test.Tasty ( defaultMain, TestTree(..) )
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen.Faker

import Test.Tasty ( TestTree(..), testGroup )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Data.Time.Calendar

import Faker
import qualified Faker.Book as Fake
import qualified Faker.Name as Fake
import qualified Faker.DateTime as Fake

import System.Random (randomRIO)
import Control.Monad (replicateM, replicateM_)
import Control.Monad.IO.Class (liftIO)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T

import Control.Exception

import System.Environment

import Records

--------------------------------------------------------------------------------
-- Generate fake books using the `fakedata` package
-- (This is using the Monad-based interface to fake data; don't worry, you'll 
--  understand how this syntax works very soon!)

fakeBook :: Fake Book
fakeBook = do
  title          <- T.unpack <$> Fake.title
  authorSurname  <- T.unpack <$> Fake.lastName
  numTags        <- liftIO $ randomRIO (1,3)
  tags           <- Set.fromList . map T.unpack 
                    <$> replicateM numTags Fake.genre
  releaseDate    <- Fake.dayBetweenYears 1900 2022
  numPages       <- liftIO $ randomRIO (100,500)
  numInitials    <- liftIO $ randomRIO (1,2)
  authorInitials <- map T.head <$> replicateM numInitials Fake.firstName
  pure $ Book {..}


--------------------------------------------------------------------------------
-- Main bulk of tests

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"

    putStrLn "Some random books for you to enjoy (after prettyPrintBook is implemented):"

    flip catch (\(e :: SomeException) -> print e) $ 
      replicateM_ 5 $ do
        b <- Faker.generateNonDeterministic fakeBook
        putStrLn $ prettyPrintBook b

    putStrLn "\n=====\n"

    muffledMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [
      testCase "Book - has been implemented correctly"
        $ let 
          b = Book "A" "BC" "D" 5 (Set.singleton "C") (read "2020-01-01")
        in and
          [ title b          == "A"
          , authorInitials b == ['B','C']
          , authorSurname b  == "D"
          , numPages b       == 5
          , tags b           == Set.fromList ["C"]
          , toModifiedJulianDay (releaseDate b) == 58849 
          ] @?= True

    ,
      testCase "programmingInHaskell - has the right data"
        $ let 
          b = programmingInHaskell
        in do
          assertBool "Title isn't quite right (check spelling/capitalisation?)"
            $ title b == "Programming in Haskell"

          assertBool "Double check the author's forename initial."
            $ authorInitials b == ['G']
          
          assertBool "Is the number of pages correct?"
            $ numPages b >= 300 && numPages b <= 330

          assertBool "Check the tags."
            $ Set.toList (tags b) == ["Haskell", "Learning"]

          assertBool "The date you have set is too early."
            $ (\(y,_,_) -> y) (toGregorian (releaseDate b)) >= 2016

          assertBool "The date you have set is too late."
            $ (\(y,_,_) -> y) (toGregorian (releaseDate b)) <= 2016

    , 
      testProperty "authorFullName - generates the right string"
        $ forAllShrink (listOf $ elements ['A'..'Z']) subterms 
        $ \is -> forAll (elements ["Adams", "Ballard", "Vonnegut"]) 
        $ \sn -> do
            let n = concatMap (\i -> i : ". ") is ++ sn
                b = Book "A" is sn 5 (Set.singleton "C") (read "2020-01-01")

            authorFullName b === n

    , 
      testProperty "prettyPrintBook - generates a string"
        $ forAll (fakeQuickcheck fakeBook) 
        $ \b -> length (prettyPrintBook b) > 50

    , 
      testProperty "hasTag - correctly ascertains the presence of a tag"
        $ forAll (fakeQuickcheck fakeBook)
        $ \b -> forAllBlind (T.unpack <$> fakeQuickcheck Fake.genre)
        $ \genre -> 
          counterexample 
          ("hasTag said that " ++ genre ++ " was not in the tags, when it was") 
          (not (genre `Set.member` tags b) || hasTag genre b)
          .&&.
          counterexample 
          ("hasTag said that " ++ genre ++ " was in the tags, when it was not") 
          (not (hasTag genre b) || genre `Set.member` tags b)

    , testProperty "addTag - adds a tag correctly"
        $ forAll (fakeQuickcheck fakeBook)
        $ \b -> forAll (T.unpack <$> fakeQuickcheck Fake.genre)
        $ \genre ->
          Set.size (tags (addTag genre b)) - Set.size (tags b)
          == if hasTag genre b then 0 else 1

    , testProperty "sameAuthor - correctly ascertains if two books have the same author"
        $ forAllBlind
          (fakeQuickcheck fakeBook) 
        $ \b -> forAllBlind (fakeQuickcheck fakeBook
            `suchThat` ((/= authorInitials b) . authorInitials))
        $ \b' -> 

          -- b'' is b with the author name of b'
          let b'' = b' 
                { authorInitials = authorInitials b
                , authorSurname = authorSurname b } in

          counterexample
          (unlines 
            [ "sameAuthor said these two books had the same author:"
            , show b
            , show b']) 
          (not $ sameAuthor b b')
          .&&.
          counterexample
          (unlines 
            [ "sameAuthor said these two books had different authors:"
            , show b
            , show b''
            ])
          (sameAuthor b b'')
          
    ]
