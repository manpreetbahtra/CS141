import Test.Tasty ( defaultMain, TestTree(..) )
import Test.Tasty.HUnit

import Test.Tasty ( TestTree(..), testGroup )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Prelude hiding (not)

main :: IO ()
main = do
    -- clear the terminal window
    putStr "\ESC[2J"
    muffledMain tests

tests :: TestTree
tests = testGroup "Tests" 
    []