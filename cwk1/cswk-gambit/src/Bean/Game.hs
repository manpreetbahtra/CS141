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
  Following from piecetypes data type, if there is a cow "Cow" is used, otherwise "Bean" is used. Also, since a piecetype consists of a colour and 
  a piecetype, relevant colour was also used. 
-}
startingPos :: Board
startingPos = [[Empty, Red Cow, Red Cow, Empty], [Red Bean, Red Bean, Red Bean, Red Bean], [Blue Bean, Blue Bean, Blue Bean, Blue Bean], [Empty, Blue Cow, Blue Cow, Empty]]


{-|
  Ex. 2: Compute the balance of material, defined as difference between the 
  number of Red beans and the number of Blue beans. Make use of explicit 
  recursion and pattern matching in your definition.

  [To compute the balance, explicit recursion and top level pattern matching is used as seen in "balance xs" and "[] (x:xs)" respectively. 
  Iterate through all the pieces in the board using explicit recursion. 
  Two other functions isRed and isEmpty are defined each of which take a piece as an input, which is retrieved from recursing through the board, and produce a 
  boolean as an output. If the piece is of colour red, isRed returns true. Using wildcard, when any other piece is passed into isRed, False is returned.
  Although the isRed will compute all the red pieces, that is red beans and red cows, but since the number of blue and red cows are equal on any board, they 
  will cancel out. Filter then takes this function as a predicate and x as the argument to be filtered. Length is then used to count the number of elements 
  that satisfied the predicate. To compute the number of blue pieces, function composition is used. isRed is composed with not function and length is calculated
  of pieces that satissy the predicate that it is blue. Upon testing the above functions, I noticed an error in my logic as -4 was being returned instead of 0 for 
  startingPos. I figured out that empty was being calculated as not.isRed. Therefore, I implemented isEmpty, which returns true if given piece is empty else false. 
  I then added the number of empty pieces so that empty was no longer being counted as not.isRed (and therefore adding to blue pieces). To make the function more 
  elegant, I could have defined two functions isRedBean and isBlueBean, which would remove the need to calculate isEmpty. 
  length (filter isRedBean x ) - length (filter isBlueBean x) + balance xs
  However, I kept the code which I was able to implement with my initial understanding. ]
-}

balance :: Board -> Int
balance []= 0
balance (x:xs)= length ( filter isRed x) - length (filter ( not . isRed ) x) + length (filter isEmpty  x ) + balance xs


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

  [List comprehension is used to iterate over each inner list, which corresponds to each row in the board. Also, I use another list comprehension to iterate
  over each piece in the row. Using the provided show Piece implementation, I show individual piece. The resulting string from the show implementation of each 
  piece is concatenated with its rows string representation using concat function. Firstly to add a new line between each row, I added "\n", however, I was unable
  to show each row on a different line, so then used unlines function. This was inserting a final new line character after the last row and failing the test, so 
  I used init function which returns all the elements from a list except the last one, hence it was not including the final newline character. implementation is 
  elegant and has meaningful names. 
  Eq instances for piece and piecetype were implemented using top level pattern matching, which return a boolean. Only a Bean/Cow is equal to itself, so Eq 
  piecetype returns true when both piecetypes are the same. Otherwise wildcards are used to represent any other case, resulting in false. 
  A piece is equal to another piece if the both pieces are of the same colour/player and same piecetype. In the case of empty (since it does not have a colour), it was
  specified separately. In any other case, it returns false.  ]
-}


instance {-# OVERLAPS #-} Show Board where
  show :: Board -> String
  show board = init $ unlines [concat [show piece | piece <- row] | row <- board ]


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

  [getPiece function takes a board and a coordinate and returns a value of type Maybe piece. Initially, I thought of checking if coordinate passed in is an element 
  of x<-{0..3}, y<- {0..3} to check if the coordinate is within bounds then just concatMap and then show (x,y) piece. This was a little confusing, so I used take 
  and drop functions. Following this, I found a function from Hoogle, which returns the element at the given index - !!. So the implementation below checks 
  if coordinated provided is within bounds, then returns Just piece. If coordinate out of bounds, Nothing is returned. Guards are used, since they provide an 
  elegant solution for this problem. ]
-}

getPiece :: Board -> Coord -> Maybe Piece
getPiece board (x,y)
    | x >=0 && y>=0 && x<=3 && y<=3 = Just $ board!!y!!x
    | otherwise = Nothing



{-| 
  Ex. 5: Return the valid moves that a cow in position (x,y) would have.

  [ Allmoves is defined seprately and used to aid elegance and readability, where allMoves is defining the top, bottom, left and right of coordinate 
  supplied. Since a cow can only move into empty spaces, if getPiece returns Just empty for any of the allMoves, that coordinate is added to the list comprehension.
    The resulting list represents the valid moves that a cow can make on the given board from the given coordinate. getPiece == Just empty acts as a predicate/filter
    for the list. ]
-}
validCowMoves :: Board -> Coord -> [Coord]
validCowMoves board (x,y) = [(a,b)| (a,b) <- allMoves (x,y), getPiece board (a,b) == Just Empty ]


allMoves :: (Num a, Num b) => (a, b) -> [(a, b)]
allMoves (x, y)= [(x-1,y), (x+1,y), (x,y-1), (x, y+1)]


{-| 
  Ex. 6: Return the valid moves for a bean on the given team in position c.

  [Case of construct is used since it allows for easy mapping between players and the opposite player beans. Coordinates from allMoves are added to a list
  if after applying that coordinate to getPiece returns empty or opposite player's bean, thus the two cases of valid moves for beans. ]
-}
validBeanMoves :: Board -> Player -> Coord -> [Coord]
validBeanMoves board player (x,y) = case player of
                    RedPlayer -> [(a,b)| (a,b)<- allMoves (x,y), getPiece board (a,b) == Just Empty || getPiece board (a,b) == Just (Blue Bean) ]
                    BluePlayer -> [(a,b)| (a,b)<- allMoves (x,y), getPiece board (a,b) == Just Empty || getPiece board (a,b) == Just (Red Bean) ]




{-| 
  Ex. 7: Set a given (valid) coordinate to have a given piece (or Empty).

  [Firstly, to implement the setCoord function I tried to use splitAt function. However since the splitAt function takes an Int, not a coordinate, 
  I had to split the lists of lists to a single list and then to a sublist. This was confusing to implement, hence decided on a different approach.
  I tried using the take and drop functions. Given a coordinate (x,y), I would take y board which was used to keep everything before the column I want to change
  the same. Then use a list comprehension to keep everything of that row the same uptil that element, using take x (board !! y), then concatenating(++) with 
  the new piece provided as input and then further concatenating with the remaining rows- drop (x+1) (board !! y) and then join remaining columns using 
  drop (y+1) board. However, to make it simpler I used a list comprehension, which goes through all the coordinates of the board and if it matches the coordinate
  passed as input, that element in the new board is set to the given piece, else it has the same piece as before. getPiece2 has a similar impplementation to 
  getPiece, with the only difference being that it returns a Piece rather than a maybe piece. Although getPiece2 is not a total function, list comprehension ensures 
  only valid coordinates are passed in. Also, for cases where a total function is more useful getPiece is supplied instead later on. 
  ] 


-}
setCoord :: Board -> Coord -> Piece -> Board
setCoord board (x,y) piece = [ [ if (a,b) == (x,y) then piece else getPiece2 board (a,b) | a <- [0..3] ] | b <- [0..3] ]

getPiece2 :: Board -> Coord -> Piece
getPiece2 board (x,y)
    | x >=0 && y>=0 && x<=3 && y<=3 = board !! y !! x



{-| 
  Ex. 8: Given two positions, return Just the updated board after moving the
  piece from the first position to the second. If the move was not valid, or 
  there was no piece in the first position, return Nothing instead.

  [For a bean to make move, the element at to coordinate (in this case (c,d)) also needs to be a bean but of opposite player. To do this, I implement typeOfElement 
  function which used top level pattern matching to check if the given piece is a bean or cow and getPieceType which returns the piecetype of the piece. used maybe 
  to make it a total function. In the first guard, pieces at both to and from need to bean but not the same piece, which means they are both beans but from different 
  players. Also, isAdjacent function is defined, which makes use of allMoves defined earlier (which is a list of top, bottom, left, right for a given coord) to check 
  if from (a,b) and to (c,d) are adjacent otherwise the move can not be made. If all 3 conditions are met, (c,d) can be changed to the element at (a,b), where the 
  element is retrieved using getPiece2 and (c,d) is changed using setCoord. This setCoord then produces a board which is taken as an input to another setCoord function 
  to set initial position (a,b) to empty.
  
  In the guard corresponding to cows, since cows can only move into empty adjacent spaces, if (c,d) is empty and is adjacent to (a,b), (c,d) is set to cow which was 
    at (a,b) and (a,b) is set to empty, in a similar fashion described above.
  If none of the two guards are met, Nothing is returned. Moreover, getPieceType checks if the coordinates supplied are within bounds. ]
-}

makeMove :: Board -> Coord -> Coord -> Maybe Board
makeMove board (a,b) (c,d)
    | pieceType1 == Just Bean && pieceType2 == Just Bean && piece1 /= piece2 && adjacents = Just $ setCoord (setCD pieceAB) (a,b) Empty
    | piece2 == Just Empty && adjacents = Just $ setCoord  (setCD pieceAB ) (a,b) Empty
    | otherwise = Nothing
    where
      pieceType1 = getPieceType board (a,b)
      pieceType2 = getPieceType board (c,d)
      piece1 = getPiece board (a,b)
      piece2 = getPiece board (c,d)
      adjacents = isAdjacent (a,b) (c,d)
      setCD = setCoord board (c,d)
      pieceAB = getPiece2 board (a,b)

getPieceType :: Board -> Coord -> Maybe PieceType
getPieceType board (x,y)
    | x >=0 && y>=0 && x<=3 && y<=3 = Just (typeOfElement (board!!y!!x))
    | otherwise = Nothing

--checks the piecetype
typeOfElement :: Piece -> PieceType
typeOfElement _Bean= Bean
typeOfElement _Cow = Cow


{-takes a coordinate and returns a bool if the coords are adjacent, and so if the move can be made. -}
isAdjacent :: Coord -> Coord -> Bool
isAdjacent (a,b) (c,d)= (a,b) `elem` allMoves (c,d)


{-| 
  Ex. 9: The game is drawn if the same setup is repeated, or after the fiftieth 
  move. Given a sequence of boards, most recent first, determine whether the 
  game is drawn in any position.

  [The function first checks if it is an empty list, in that case it cannot be a draw and returns False. It looks for repetitions of the same board position 
  in the history using drawByRepetition which checks if currentBoard x is an element of the previous boards xs. If it is element, it means it is a repeat board, 
  in that case it returns true, and thus gameIsDrawn function returns true. drawByRepetition recursively checks, if head is not an element of history, it checks with the tail of the list. 
  Another predicate is draw by exhausation, which can be calculated using the length of history of moves. 51 is used instead of 50, because history will 
  also contain startingPos, but it was not a move made by the player.]
-}

gameIsDrawn :: [Board] -> Bool
gameIsDrawn [] = False
gameIsDrawn (x:xs) = drawByRepetition x xs || length (x:xs) >=51 --exhaustion

drawByRepetition :: Board -> [Board] -> Bool
drawByRepetition x xs = x `elem` xs || gameIsDrawn xs



{-| 
  Ex. 10: The game is won by Red if, on Blue's turn, one of their cows is 
  unable to move - and vice versa. Given a board, and the player whose move it 
  is, determine whether the game is won by the opponent.

  [To check if cow is trapped, cows coordinates must be found out. To implement this getCow function takes in a player and board, and if 
  the player is red, it calls containsTheGivenPiece on red cows, which produces a list of coordinates on which red cows are found and vice versa.
  This list is then passed as cowCoords to gameIsWon function. Since there are only 2 cows for 1 player, head and last function can be applied to 
  get the coordinates of each of the cows individually. Then these coordinates can be passed into validCowMoves, which returns a list of coordinates 
  in which that cow can move into. If the length of that list for either of the cows is 0, it means that the cow is trapped and other player has won. 
  Also, an Eq instance is defined for player using pattern matching, so that it could check in getCow if given player was equal to a specific player.]
-}

gameIsWon :: Board -> Player -> Bool
gameIsWon board player

  | null (validCowMoves board cowCoord1) || null (validCowMoves board cowCoords2) = True
  | otherwise = False
    where
      cowCoords = getCow board player
      cowCoord1 = head cowCoords -- where my cow is 
      cowCoords2 = last cowCoords -- where my 2 cow is 


{-given a player, we have its cows list of coordinates-}
getCow :: Board -> Player -> [Coord]
getCow board player
  | player == RedPlayer = containsTheGivenPiece board $ Red Cow
  | player == BluePlayer = containsTheGivenPiece board $ Blue Cow

{-list of coords of a given piece-}
containsTheGivenPiece :: Board -> Piece -> [Coord]
containsTheGivenPiece board piece = [(x,y) | x <- [0..3], y<- [0..3], getPiece2 board (x,y) == piece ]

instance Eq Player where
    RedPlayer == RedPlayer = True
    BluePlayer == BluePlayer = True
    _ == _ = False

{-| 
  Finale: Implement a computer player to play the game of Bean's Gambit.
  Given the history of the game as a (nonempty) list, the AI should return an 
  appropriate next move as a pair (from, to).
  {-cited resource for grand finale  - https://towardsdatascience.com/how-a-chess-playing-computer-thinks-about-its-next-move-8f028bd0e7b1-}

  [To implement an Artitifically Intelligent player, my strategy was to 
    1. Capture opponents cow 
    2. Protect own cow
    3. Capture opponents bean
    4. Protect own beans
    This particular was assigned because the player is more likely to win if moves are made in this order. If it can capture opponents cow, it has immediatedly won.
    If it doesn't protect its own cow, it will lose. If it can capture other player's beans it is more likely to win since it cows are less likely to be trapped 
    and then it should protect its own beans. These were thought of as weightings/utilites, which I derived from the cited resource. However, due to insufficient
    time, I was only able to implement protect own cow and capture opponents bean approach. Each approach is discussed below.   ]
-}

nextMove :: [Board] -> Player -> (Coord, Coord)
nextMove [board] player
 -- | length $ validCurrentCowMoves board player ==1 = protectMyCow board player
    |length (validCowMoves board from) == 1 = (from, to )                                          --if first cow only has 1 valid move, save it
    |length (validCowMoves board to) == 1 = (secondCowFrom, secondCowTo )                         --if second cow only has 1 valid move, save it
    | not (null x) = head x                                                                     -- capture beans 
    |otherwise = head $ fromToBeans board player
  where
    x = possibleBeanCapture board player
    from = head $ locateCows board player
    to = head $ validCowMoves board from
    secondCowFrom = last $ locateCows board player
    secondCowTo = last $ validCowMoves board secondCowTo





{-1 approach protect own cow. For this I made 8 functions. Since each player has two cows, if a cow has only 1 valid move, in other words only one empty 
  space surrounding it, it is likely to trapped soon. 
  locateCows - produces a list of coordinates where the player's cows are located 
  locateBeans - produces a list of coordinates where the player's beans are located
  After locating player's beans and cows, compute the list of their valid moves. This is done using validCurrentBeanMoves and validCurrentCowMoves. 
  validCurrentBeanMoves and validCurrentCowMoves produces a mixed list of all the moves that can be made be any bean or cow. 
  However, for nextMove to work, a from to pair is needed. Hence, to create from To pairs, fromToBeans and fromToCows functions ae created.  
  Then these functions are assembled to create a protectMyCow function. 
  Since there are only 2 cows, I used head and last to extract the coordinate of where the first cow is located and the coordinate of where the second/last 
  cow is located on locateCows. 
  validCowMoves takes board and the (player's)cow's coordinate to compute how many valid moves that cow has. If only 1, cow should be protected from trapping. 
  -}


{-If a cow only has one valid move it can make, it makes the move from the cows coordinate to the one  empty coordinate it can move to-}
protectMyCow :: Board -> Player -> Maybe Board
protectMyCow board player
    |length (validCowMoves board from) == 1 = makeMove board from to
    |length (validCowMoves board to) == 1 = makeMove board secondCowFrom secondCowTo
    |otherwise = Nothing
    where
      from = head $ locateCows board player
      to = head $ validCowMoves board from
      secondCowFrom = last $ locateCows board player
      secondCowTo = last $ validCowMoves board secondCowTo





{-from this function i will have the coords of my beans -}
locateBeans :: Board -> Player -> [Coord]
locateBeans board player= case player of
  RedPlayer -> containsTheGivenPiece board $ Red Bean
  BluePlayer -> containsTheGivenPiece board $ Blue Bean

{-from this function i will have the coords of my cows-}
locateCows :: Board -> Player -> [Coord]
locateCows board player= case player of
  RedPlayer -> containsTheGivenPiece board $ Red Cow
  BluePlayer -> containsTheGivenPiece board $ Blue Cow

 {-after locating my beans and cows, i am trying to compute the list of their valid moves.-}
validCurrentCowMoves ::  Board -> Player -> [Coord]
validCurrentCowMoves board player = filter (isValidMove board ) cowCoords
  where
    cowCoords = locateCows board player                                                   --running it on the cows the player has
    isValidMove board coord = coord `elem` validCowMoves board coord                      -- checking validcowmives 

{-computing a list of valid moves for the bean-}
validCurrentBeanMoves :: Board -> Player -> [Coord]
validCurrentBeanMoves board player = filter (isValidMove board ) beanCoords              -- out of all beans, which has valid moves.  
  where
    beanCoords = locateBeans board player                                                --running it on the beans the player has
    isValidMove board coord = coord `elem` validBeanMoves board player coord
      -- checking validbeanmoves. if the coordinate is in the validbean moves, then it is valid. 

{-trying to create from to pair-}
isMoveValid :: Board -> Coord -> Coord -> Bool
isMoveValid board (a,b) (c,d)
      | getPieceType board (a,b) == getPieceType board (c,d) && getPiece2 board (a,b) /= getPiece2 board (c,d) && isAdjacent (a,b) (c,d) = True
      | getPiece2 board (c,d) ==  Empty && isAdjacent (a,b) (c,d) = True
      | otherwise = False


{-trying to create a from to pair for cows-}
fromToCows :: Board -> Player -> [(Coord, Coord)]
fromToCows board player =
  let
    -- Get the coordiantes of the player's cows
    cowCoords = locateCows board player

    -- Get the valid moves for the player's cows
    newCowCoords = validCurrentCowMoves board player

   -- Generate a list of tuples representing valid moves 
    pairs = [(from, to )| from <- cowCoords, to <- newCowCoords, isMoveValid board from to ]

  in

    pairs


{-trying to create a from to pair for beans-}
{- another approach I used for FromToBeans was the following, however to make it clearer I used a let clause. 
[(from, to ) | from <- locateBeans board player, to <- validCurrentBeanMoves board player, isMoveValid board from to]  -}


-- Generate a list of valid moves for the player's beans on the board
fromToBeans :: Board -> Player -> [(Coord, Coord)]
fromToBeans board player =
    let
        -- Get the coordinates of the player's beans
        beanCoords = locateBeans board player

        -- Get the valid moves for the player's beans
        newBeanCoords = validCurrentBeanMoves board player

        -- Generate a list of tuples representing valid moves
        pairs = [(from, to) | from <- beanCoords, to <- newBeanCoords, isMoveValid board from to]
    in
        pairs





{- 2nd approach capture opponent's bean
1. checkBeanCaptureOppositeBean produces a list of coordinates such that surrounding the player's bean is the opposite bean, which means it can capture it.
2. To create from to pairs of bean at which location can move into which location/s to capture opposite beans, possibleBeanCapture function is used.
3. captureBean function destructures the list of pairs of coordinates from and to which bean can move. It uses the head to get the first pair. Since, 
the pair is a tuple, to decompose it further I used fst and snd to extract from and to coordinates respectively. This can then be passed to nextMove function. 
-}




{-now i have a list of coords such that surrounding my bean is the opposite bean 
uses case of construct, which allows for easy mapping between players and opposite bean. -}
checkBeanCaptureOppositeBean :: Board -> Player -> Coord -> [Coord]
checkBeanCaptureOppositeBean board player (x,y) = case player of
                    RedPlayer -> [(a,b)| (a,b)<- allMoves (x,y), getPiece2 board (a,b) == Blue Bean ]
                    BluePlayer -> [(a,b)| (a,b)<- allMoves (x,y), getPiece2 board (a,b) == Red Bean ]

{-list of coords that all my beans can make to capture opposite beans -}
checkSurroundings :: Board -> Player -> [Coord]
checkSurroundings board player = filter (checkBeanCaptureOppositeBean board player ) beanCoords  -- out of all beans, which has valid moves.  
  where
    beanCoords = locateBeans board player --running it on the beans the player has
    checkBeanCaptureOppositeBean board player coord = coord `elem` validBeanMoves board player coord

{-trying to create from to pair-}
isMoveValidToCaptureBean :: Board -> Coord -> Coord -> Bool
isMoveValidToCaptureBean board (a,b) (c,d)
      | getPieceType board (a,b) == Just Bean && getPieceType board (c,d) == Just Bean  && getPiece2 board (a,b) /= getPiece2 board (c,d) && isAdjacent (a,b) (c,d)  = True
      | otherwise = False

{-we need to check if the two coords are adjacent -}
{-producing a list of from to coords such that my bean can capture the other bean at to location-}
possibleBeanCapture :: Board -> Player -> [(Coord, Coord)]
possibleBeanCapture board player = [(from, to ) | from <- beanCoords, to <- oppositeBeanCoords, isMoveValidToCaptureBean board from to]
  where
    beanCoords = locateBeans board player
    oppositeBeanCoords = checkSurroundings board player

{-capturing a bean -}
captureBean :: Board -> Player -> Maybe Board
captureBean board player = makeMove board from to
  where
    from = fst $ head $ possibleBeanCapture board player
    to = snd $ head $ possibleBeanCapture board player






{-3rd approach. I tried to implement the case where opponent's cow can be trapped.  To do this, I attempted to compute, if making a move would bring it closer 
  to the opponent's cow. I tried to do this by checking if the bean's current index is 2 (up,down,left,right) away from opposite cow. If so, it 
    should move in that direction. This implementation does not yet which of the possible moves take it closer to opposite cow to trap it. If given more time, 
    I could successfully compute so. -}

{-now i need to traverse the list of coords i have and check if TBLR of it is a cow-}
{- checkBeanSurroundedByCow board (x:xs) = map (getPiece2 board x) (locateBeans board player) : checkBeanSurroundedByCow xs 
  where 
    x = [(x,y)| (x,y)<-(x-1,y), (x+1,y), (x,y-1), (x, y+1)] -}
{- checkBeanSurroundedByCow board player = locateFirstBeanX
  where
    findOppositeCows = if player == RedPlayer then locateCows board BluePlayer else locateCows board RedPlayer
    locateFirstBean = head $ locateBeans board player 
    locateFirstBeanX= if locateFirstBean `elem` canMyBeanBecomeFirstCowNeighbour (x,y) && isMoveValidToCaptureCow then (locateFirstBean, locateFirstCow) else removeElem locateFirstCow findOppositeCows
    (x,y) = head $ findOppositeCows
    canMyBeanBecomeFirstCowNeighbour (x,y)= filter getPiece2 board [(x-2, y), (x+2,y), (x,y+2), (x,y-2) ] == Empty || 
    isMoveValidToCaptureCow board (locateFirstBean) ()

removeElem :: Eq a => a -> [a] -> [a]
removeElem elem xs = filter (/= elem) xs -}
