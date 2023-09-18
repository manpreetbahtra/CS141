module Monoids where

{-

WHAT?! There's nothing here!

You will need to construct a solution to this lab sheet all by yourself. 
You have control.

Read the lab sheet carefully --- everything that's needed is explained. 
Make use of what we have seen in lectures.

If you get stuck, contact a lab tutor.

Good luck!

-}

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.IO.Handle (hClose_help)

newtype Wheel = Wheel (Map Char Char )
    deriving Show

--help
makeWheel :: [(Char,Char)] -> Wheel
makeWheel [] = []
makeWheel (x:xs)= Map.fromList x : makeWheel xs 


