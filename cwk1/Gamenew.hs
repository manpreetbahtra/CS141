{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Bean.Game where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 2208321
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

import Bean.Types



{-| 
  Ex. 1: Write an expression representing the starting position for the game.

  As detailed in the type description, Board type is a list of lists, where each nested list corresponds to one row of the board.
  I then defined each row using the pieces it contained. Since the dots represent empty square, Empty was used from Piece data type. 
  Following from piecetypes data type, if there is a cow "Cow" is used, otherwise "Bean" is used.
-}
startingPos :: Board
startingPos = [[Empty, Red Cow, Red Cow, Empty], [Red Bean, Red Bean, Red Bean, Red Bean], [Blue Bean, Blue Bean, Blue Bean, Blue Bean], [Empty, Blue Cow, Blue Cow, Empty]]


{-|
  Ex. 2: Compute the balance of material, defined as difference between the 
  number of Red beans and the number of Blue beans. Make use of explicit 
  recursion and pattern matching in your definition.

  [balance of material = number of Red Bean - number of blue bean] + because empty were being counted as blue. talk about elegancy
-}
balance :: Board -> Int
balance []= 0
balance (x:xs)= length (filter (isRed) x) - length (filter ( not . isRed )x) + length (filter (isEmpty ) x ) + balance xs
isRed :: Piece -> Bool
isRed (Red _) = True
isRed _ = False
isEmpty :: Piece -> Bool
isEmpty Empty = True
isEmpty _ =False


{-| 
  Ex. 3: Implement a 'Show' instance for 'Board'. 
  The rendered version should be exactly as it appears in the specification.
  
  Also implement an 'Eq' instance for 'Piece' and for 'PieceType'.
  You should NOT derive these automatically.

  [use some library from base to make it simpler]
-}


instance {-# OVERLAPS #-} Show Board where
  show :: Board -> String
  show board = init(unlines ( map (concatMap show) board))


instance Eq PieceType where
    Bean == Bean = True
    Cow == Cow = True
    _ == _ = False

instance Eq Piece where
    Empty == Empty = True
    Red Cow == Red Cow = True
    Blue Cow == Blue Cow = True
    Red Bean == Red Bean = True
    Blue Bean == Blue Bean = True
    _ == _ = False


{-| 
  Ex. 4: Implement a function that gets a piece at a coordinate. 
  If the location is off the board, return Nothing, otherwise return the 
  appropriate piece (or Empty), wrapped in a Just.

  [JUSTIFY]
-}
getPiece :: Board -> Coord -> Maybe Piece
getPiece board (x,y)
    | x >=0 && y>=0 && x<=3 && y<=3 = Just (board!!y!!x)
    | otherwise = Nothing
{-WHat i thought for ex4
if {(x,y) | x<- {0..3}, y<- {0..3}} then "Just" + show (x,y) piece
else Nothing -}


{-| 
  Ex. 5: Return the valid moves that a cow in position (x,y) would have.

  [JUSTIFY]
-}
validCowMoves :: Board -> Coord -> [Coord]
-- validCowMoves board (x,y) = filter(map(getPiece board (x, y) == Just Empty | x <- [x-1, x+1], y <- [y-1, y+1]))
validCowMoves board (x,y) = [(a,b)| (a,b)<- [(x-1,y), (x+1,y), (x,y-1), (x, y+1)], getPiece board (a,b) == Just Empty ]
-- filter (getPiece . map [x <- [x-1, x+1], y <- [y-1, y+1]] == Empty)
-- [(x + 1, y) | getPiece board (x + 1, y) == Just Empty]



{-| 
  Ex. 6: Return the valid moves for a bean on the given team in position c.

  [JUSTIFY]
-}
validBeanMoves :: Board -> Player -> Coord -> [Coord]
validBeanMoves board player (x,y) = case player of 
                                RedPlayer -> [(a,b)| (a,b)<- [(x-1,y), (x+1,y), (x,y-1), (x, y+1)], getPiece board (a,b) == Just Empty || getPiece board (a,b) == Just (Blue Bean) ]
                                BluePlayer -> [(a,b)| (a,b)<- [(x-1,y), (x+1,y), (x,y-1), (x, y+1)], getPiece board (a,b) == Just Empty || getPiece board (a,b) == Just (Red Bean) ]


{-| 
  Ex. 7: Set a given (valid) coordinate to have a given piece (or Empty).

  [JUSTIFY] take y board- is used to keep everything before the column i want to change the same. 
  take x (board !! y)-- used to keep everything of that row the same uptil that element.
  [piece]- change the piece to the new piece.
  drop (x+1) (board !! y)- the remaining rows
  drop (y+1) board - remaining columns


-}
setCoord :: Board -> Coord -> Piece -> Board
-- setCoord board (x,y) piece = splitAt x && splitAt y 
setCoord board (x,y) piece = take y board ++ [take x (board !! y) ++ [piece] ++ drop (x+1) (board !! y)] ++ drop (y+1) board 




{-| 
  Ex. 8: Given two positions, return Just the updated board after moving the
  piece from the first position to the second. If the move was not valid, or 
  there was no piece in the first position, return Nothing instead.

  [JUSTIFY]
-}

makeMove :: Board -> Coord -> Coord -> Maybe Board
{-check if the coordinates given are in bounds- like in getPiece if the first piece is not empty
  then set coordinate, just show board else if first piece empty or out of bounds- nothing -}
makeMove board (a,b) (c,d)
        | getPieceType board (a,b) == getPieceType board (c,d) && getPiece board (a,b) /= getPiece board (c,d) = Just (setCoord (setCoord board (c,d) (getPiece2 board (a,b))) (a,b) Empty) 
        | getPiece board (c,d) == Just Empty = Just(setCoord  (setCoord board (c,d) (getPiece2 board (a,b))) (a,b) Empty)
        | otherwise = Nothing 

getPieceType :: Board -> Coord -> Maybe PieceType
getPieceType board (x,y)
    | x >=0 && y>=0 && x<=3 && y<=3 = Just (typeOfElement(board!!y!!x))
    | otherwise = Nothing

--checks the piecetype
typeOfElement :: Piece -> PieceType
typeOfElement _Bean= Bean
typeOfElement _Cow = Cow 

getPiece2 :: Board -> Coord -> Piece
getPiece2 board (x,y) 
    | x >=0 && y>=0 && x<=3 && y<=3 = board !! y !! x


{-| 
  Ex. 9: The game is drawn if the same setup is repeated, or after the fiftieth 
  move. Given a sequence of boards, most recent first, determine whether the 
  game is drawn in any position.

  [JUSTIFY]
-}

gameIsDrawn :: [Board] -> Bool
{- gameIsDrawn history 
  | length history >=51 = True -- draw by exhausation
  | drawByRepetition board history = True -- draw by repetition
  | otherwise = False  -}


gameIsDrawn [] = False
gameIsDrawn (x:xs) = drawByRepetition x xs || length (x:xs) >=51
-- gameIsDrawn (x:xs) = length (x:xs) >=51

drawByRepetition :: Board -> [Board] -> Bool
drawByRepetition x xs = x `elem` xs || gameIsDrawn xs 



{-| 
  Ex. 10: The game is won by Red if, on Blue's turn, one of their cows is 
  unable to move - and vice versa. Given a board, and the player whose move it 
  is, determine whether the game is won by the opponent.

  [JUSTIFY]
-}
gameIsWon :: Board -> Player -> Bool 
{- gameIsWon board player 
  | length (validCowMoves board (indexOfCoordinates (getCowCoordForTheGivenPlayer board player) 0 )) == 0 || length (validCowMoves board (indexOfCoordinates (getCowCoordForTheGivenPlayer board player) 1 )) == 0  = True
  | otherwise = False  -}

gameIsWon board player
    | newFunction board player = True
    | otherwise = False 

newFunction :: Board -> Player -> Bool
newFunction board player =length [somethingElse] == 0

somethingElse :: Board -> [Coord] -> [Coord]
somethingElse [] = [] 
somethingElse board [(x,y):(xs,ys)]= validCowMoves board (x,y) : somethingElse board (xs,ys) -- validCowMoves board (getCowCoordForTheGivenPlayer board player) 
{- indexOfCoordinates :: (Board -> Player ->[Coord]) -> Int -> Coord
indexOfCoordinates (getCowCoordForTheGivenPlayer board player) index = getCowCoordForTheGivenPlayer board player !! index  -}

{-given a player, we have its cows list of coordinates-}
getCowCoordForTheGivenPlayer :: Board -> Player -> [Coord]
getCowCoordForTheGivenPlayer board player 
  | player == RedPlayer = containsTheGivenPiece board (Red Cow)
  | player == BluePlayer = containsTheGivenPiece board (Blue Cow)

{-list of coords of a given piece-}
containsTheGivenPiece :: Board -> Piece -> [Coord]
containsTheGivenPiece board piece = [(x,y) | x <- [0..3], y <- [0..3], getPiece2 board (x,y) == piece ] 


{-| 
  Finale: Implement a computer player to play the game of Bean's Gambit.
  Given the history of the game as a (nonempty) list, the AI should return an 
  appropriate next move as a pair (from, to).

  [JUSTIFY]
-}
nextMove :: [Board] -> Player -> (Coord, Coord)
nextMove = error "Not implemented"