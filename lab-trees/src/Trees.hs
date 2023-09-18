module Trees where
import Debug.Trace (traceEvent)

    --import Data.UTree (UTree)
    --import qualified Data.UTree as UTree 

-- Complete this according to the lab sheet.
-- Make sure to complete ALL the exercises.


-- Import UTree qualified.


-- Define nubSort in terms of functions over the UTree data type. (You may need to import Data.Foldable.)
nubSort :: Ord a => [a] -> [a]
nubSort xs = foldr (:) [] tree
    where
        tree = foldr  insert empty xs


-- Define a type synonym for UTree called Set.
