module Records where

--------------------------------------------------------------------------------
-- For this lab, the imports are defined for you.
-- Notice the use of epxlicit import lists.

import Data.Time.Calendar (Day)
import Data.List (intercalate, intersperse)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Function (on)
import GHC.Real (reduce)

--------------------------------------------------------------------------------
-- Define your data types and functions here.

data Book = Book {
    title :: String,
    authorInitials :: [Char],
    authorSurname :: String,
    numPages :: Integer,
    tags :: Set.Set String,
    releaseDate :: Day

}deriving (Show)

programmingInHaskell :: Book
programmingInHaskell = Book {
    title = "Programming in Haskell",
    authorInitials = ['G'],
    authorSurname = "Hutton",
    numPages = 320,
    tags = Set.fromList ["Haskell", "Learning"],
    releaseDate = read "2016-09-01" 
}

authorFullName :: Book -> String
authorFullName book  = concatMap addDot initials ++ authorSurname book
    where
        addDot a = [a, '.', ' ']
        initials = authorInitials book 

prettyPrintBook :: Book -> String
prettyPrintBook b = show b
--could also do prettyPrintBook = show --eta reduce

hasTag :: String -> Book -> Bool
hasTag tag b = Set.member tag (tags b)

addTag :: String -> Book -> Book
addTag tag b 
    | hasTag tag b == False = b {tags = Set.insert tag (tags b)}
    | otherwise = b 
-- updateFirstName fn s = s { firstName = fn }

sameAuthor :: Book -> Book -> Bool
sameAuthor a b = (==) `on` authorFullName a authorFullName b 

