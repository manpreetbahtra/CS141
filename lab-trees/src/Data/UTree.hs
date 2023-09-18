module Data.UTree ( UTree(), empty, singleton, insert, fromList, member, foldr ) where

-- Complete this according to the lab sheet.
-- Make sure to complete ALL the exercises.

-- NOTE: The error messages may not be too easy to read, due to the constructors not being exposed to the tests. In error messages, lists of values represent UTrees constructed by passing that list to the fromList function.

--------------------------------------------------------------------------------
-- Data Type
{-Two constraints- No repeated elements. Tree is sorted/in order. elements to the left of the node are smaller than it. Elements to the right are greater than it.-}
-- A unique tree.
data UTree a = Empty | Node a (UTree a) (UTree a) 
-- one bracket Utree a is the left subtree because left subtree is also of type UTree a and in the other bracket, right subtree is also a subtree so it must be of type Utree too. 


-- Smart constructors

-- An empty tree.
empty :: Ord a => UTree a
empty = Empty


-- A tree with a single value. if its a singleton, it doesnt have left or right subtrees
singleton :: Ord a => a -> UTree a
{-what i did 
singleton a = Node a UTree a UTree a 
but its a singleton, it does not have left or right. so left and right should be empty-}
singleton a = Node a Empty Empty

--------------------------------------------------------------------------------
-- Useful functions

-- Insert a value into an existing tree.
insert :: Ord a => a -> UTree a -> UTree a
insert x Empty = singleton x
insert x (Node v l r)
  | x == v = Node v l r 
  | x < v = Node v (insert x l ) r 
  | x > v = Node v l (insert x r )



-- Turn a list of values into a UTree.
fromList :: Ord a => [a] -> UTree a
fromList  = foldr insert empty 

member :: Ord a => a -> UTree a -> Bool
member x Empty = False
member x (Node v l r)
  | x ==v = True
  | x < v = member x l -- l in itself is a node v l r type. it is also a utree
  | x > v = member x r 


-- Define a Foldable instance for UTree.
instance Foldable UTree where
  -- foldr :: (a -> b -> b) -> b -> OrdTree a -> b
  --it takes a function, a base case and the type of tree
  foldr f z Empty = z
  foldr f z (Node n l r ) = a3
    where
      a1 = foldr f z r --folding from the right, so you want the right most node, which will (ofcourse) be in the right subtree
      a2 = f n a1 -- i think you want to apply the function to all the elements you found in the right tree.
      a3 = foldr f a2 l -- now in the left subtree, you want to start from its right most node. 

{-Explanation from internet 
  For the Node case, we traverse the tree in-order by first folding over the right subtree with foldr f z r - in a1. 
  We then apply the binary operator f to the current value n and the result of folding over the right subtree. 
  Finally, we fold over the left subtree by recursively calling foldr with f and the accumulated result (a2 accumulated result).    -}
