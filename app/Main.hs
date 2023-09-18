module Main (main) where

--------------------------------------------------------------------------------
-- This is where you should put the main application logic for your program.
--------------------------------------------------------------------------------
import Data.Char 
import System.Environment (getArgs)
import Bean.Game
import Bean.Types
import Data.Void
import Data.Maybe(fromJust)
import Text.Megaparsec
import Control.Monad
import Text.Megaparsec.Char (digitChar, char)
import qualified Data.Text as T


type Parser = Parsec Void String



-- Given a character, return a PieceType if it represents a valid piece, otherwise return Nothing.
charToPieceType :: Char -> Maybe PieceType
charToPieceType c = case toUpper c of
  'B' -> Just Bean
  'C' -> Just Cow
  _   -> Nothing


-- Given a character, return a Maybe Player if it represents a valid player, otherwise return Nothing.
charToPlayer :: Char -> Maybe Player
charToPlayer c = case toUpper c of
  'R' -> Just RedPlayer
  'B' -> Just BluePlayer
  _   -> Nothing



-- parsing board by parsing each row where each row further parses each cell. 
parseBoard :: Parser Board
parseBoard = do
  rows <- replicateM 4 parseRow
  let board = map (map fromJust) rows
  guard (isValidBoard board)
  return board

-- parse each row which calls parsePiece where each piece is parsed 
parseRow :: Parser [Maybe Piece]
parseRow = replicateM 4 parsePiece


--choice is used here in the monadic context, that allows parseEmpty, parseRed or parseBlue to occure depending on which type of piece was in the cell. 
parsePiece :: Parser (Maybe Piece)
parsePiece = choice [parseEmpty, parseRed, parseBlue]
  where
    parseEmpty = string "." >> return Nothing
    parseRed = string "R" >> Just . Red <$> parsePieceType
    parseBlue = string "B" >> Just . Blue <$> parsePieceType
    parsePieceType = choice [string "B" >> return Bean, string "C" >> return Cow]

-- <$> operator from the Applicative typeclass is used to apply parseRed, parseBlue parsers to combine  Just with the result of the parsePieceType parser.


--either - right - happy path. If there is a parsing error, Left is returned with an error message. 
--If the parsing is successful, it returns a Right with the resulting Board object.
--takes a string input and returns an either type which could be Left containing an error message as a string, or a Right containing a Board object.
--T.strip function is used to remove any leading or trailing white space from the input string, and T.pack and T.unpack are used to convert between Text and String types.
--these are imported from data.text. 

parseStartingPos :: String -> Either String Board
parseStartingPos input = case parse parseBoard "" (T.unpack $ T.strip $ T.pack input) of
  Left err -> Left ("Error parsing board: " ++ show err)
  Right board -> Right board




-- predicate that checks if the length/size of board is 4*4(16)
-- takes a Board as input and returns a Bool indicating whether or not the board is valid. If total number of pieces on the board is equal to 16, it is valid.
-- it flattens the Board using concat, which turns Board (a list of list) into a single list. 
-- It then checks whether the length of the flattened board is equal to 16. If it is, the function returns True. Otherwise, it returns False.
isValidBoard :: Board -> Bool
isValidBoard board = length (concat board) == 16





--mapM char applies the char function (which parses a single character) to each character in the input string, and returns a Parser that will parse the entire string. 
string :: String -> Parser String
string = Main.mapM char


--It applies the given function f to each element of the input list x:xs. Recursively applying f to xs, and then concatenating the results using the return function.
mapM f [] = return []
mapM f (x:xs) = do
    x' <- f x
    xs' <- Main.mapM f xs
    return (x':xs)

--parser that extracts an Integer value. some combinator applies the parser one or more times and returns the list of results. 
--some digitChar parses one or more digit characters, and read function converts the parsed string of digits into an Integer. The <$>/fmap 
-- operator is used to chain/apply the read function to the parsed string of digits.
parseInteger :: Parser Integer
parseInteger = read <$> some digitChar



{- It first reads the name of a file to parse from the command line arguments using getArgs and assigns it to the name variable. 
It then gets the first element of name using head and assigns it to fileToParse.
Then reads the contents of the file using readFile and prints the contents of the file to the terminal using putStrLn. 
listOfChars uses the lines function to split the input string input into a list of strings, where each string is a line of the input. It also flattens 
the contents of the file which is on different file numbers into 1 list, separated by line numbers. Then it maps the parse1Line 
function over each line of the input to parse it into a Maybe Piece value, resulting in a list of Maybe Piece values.
Following this, the starting board is printed. 

From the input file, line numbers are then checked. number checks if the line numbers do not appear at the start of the line.
  order is the predicate which checks the line numbers are followed by a dot. If both conditions are met, then an either type is returned. 
  In case of left, there was failure to parse the integers in the input file and in case of right, the parsed result from parsed integer is
  printed.  
  
  If the line numbers were not in sequential order, "Incorrect line numbers' is printed and if the numbers are not in sequential oder incorrect line numbers is printed. 
  Filter the unnecessary characters. The use an either expression to handle the result of review. 

  nextMoves is a list of coordinate pairs representing the moves in the game. It is obtained by concatenating the result of reviewed, which is a list of lists of 
  coordinate pairs. Since the list is flattened, nextMoves will contain all the coordinate pairs, in the order they were parsed from the input string.
    
   -}


main :: IO()
main = do 
    name <- getArgs 
    let fileToParse = head name
    input <- readFile fileToParse 
    putStrLn input
    

    let listOfChars = map parse1Line ( lines input)
    putStrLn (show listOfChars)
    print startingPos


    let lines' = lines input
    -- check that the first character in every line is a number (should not need to do for valid files)

    let number = all (\line -> isDigit (head line)) lines'
    --check that the numbers are in sequential order followed by a dot

    let order = all (\(i, line) -> (show i  ++ ".") == takeWhile (/= ' ') line) (zip [1..] lines')

    
    if number && order then
        case parse parseInteger "" input of
        Left err -> putStrLn ("Failed to parse integer: " ++ show err)
        Right n -> print n
    else
        if order == False
            then putStrLn "Incorrect line numbers"
        else 
            putStrLn "No dots after line numbers "    
    
    --line numbers checking 


    
-- It takes a list of characters, filters out some characters using unnecessary function, concatenates the remaining characters, and then prints the result to the console.
    let filteredContents = unnecessary (concat listOfChars)
    putStrLn (filteredContents)


    let reviewed = review filteredContents 
    either print (putStrLn . show) (reviewed)


    let nextMoves = concat reviewed


    answer <- foldM produceNewBoard startingPos nextMoves
    
    showBoard answer
    return ()




parse1Line :: String -> [Char]
parse1Line line = map parseAChar line

parseAChar :: Char -> Char
parseAChar a = a 



{- Removes unwanted characters. Used pattern matching. In the case of an empty string, and empty string should be returned.
In the recursive case, it checks if the first character x is a number and if the second character head xs is a dot. This is used to check for line numbers.  
If both conditions are true, thena recursive call is made on the remaining characters.
Also, it removes any leading whitespace characters removed. This is achieved using the dropWhile isSpace $ tail xs expression, 
which drops whitespace characters from the beginning of xs and returns the remaining substring after the first character.
If the above conditions are not met, which means there was not a line number or a dot, the function checks if the first character x is a space or carriage return character \r. 
  (this is the case when the line number has been checked.)
  If it is, then it make a recursive calls on the remaining characters. 
  the function removes some characters from a string, specifically any number followed by a dot (with any leading whitespace removed) and any space or carriage return characters. -}

unnecessary :: String -> String
unnecessary [] = []
unnecessary (x:xs)
  | isNumber x && not (null xs) && head xs == '.' = unnecessary $ dropWhile isSpace $ tail xs
  | otherwise = if x == ' ' || x == '\r' then unnecessary xs else x : unnecessary xs


-- function to evaluate the input string. either type so that if successul, right is returned otherwise left. It calls upon parseForInput parser which converts the input into tuples.
review :: String -> Either String [[(Int, Int)]]
review input = case parse parserForInput "" input of
  Left error -> Left $ errorBundlePretty error
  Right answer -> Right answer



--It's a parser for a list of lists of tuples, where each tuple contains two integers.This is to convert each the characters into a position on the board.
-- used do notation to sequence the parser actions. Many combinator is used to parse 0 or more occurences since there might be no valid coord or parserForDrawLose. 
--turnIntoCoord creates a tuple of the index position. Either the characters are converted to coordinates using the coord parser or a draw or lose character hence 
-- parserForDrawLose is used. Since do is monadic IO operation return is used to call the helper function. 

parserForInput :: Parser [[(Int, Int)]]
parserForInput = do 
    cells <- many $ choice [coord, parserForDrawLose]
    return (turnIntoCoord cells)
    where 
      turnIntoCoord [] = []
      turnIntoCoord [x] = [[x]] 
      turnIntoCoord (x:y:xs) = [x,y] : turnIntoCoord xs 
-- if theres d or # it should be kept separate as [[x]] does. 


-- coord parser
{- Parser for coordinates in the form of (letter,number) where letter is one of 'a' to 'd' and number is one of '1' to '4'.
It first matches the opening parenthesis character '(' since in the list the coordinates/input have parenthesis around them.
I then use the oneOf function to match a character in the range 'a' to 'd', since teh x coordinate is from a b c or d. 
Followed by a comma character ',' and then matche a character in the range '1' to '4' using oneOf function, since y is an elemant of 1,2,3,4.
At the end, I match the closing parenthesis character ')' to complete the coordinate.
The parser then returns a tuple representing the corresponding row and column of the coordinate on a 4x4 grid. 
The mapping of letter-number combinations to row-column coordinates is defined in a case expression using pattern matching.
If the input does not match the expected format or contains an invalid coordinate, an error message is thrown using the error function. -}


coord :: Parser (Int, Int)
coord = do 
    char '('
    x <- oneOf ['a'..'d']
    char ','
    y <- oneOf ['1'..'4']
    char ')'
    return $ case (x,y) of 
        ('a', '1') -> (0,3)
        ('a', '2') -> (0,2)
        ('a', '3') -> (0,1)
        ('a', '4') -> (0,0)
        ('b', '1') -> (1,3)
        ('b', '2') -> (1,2)
        ('b', '3') -> (1,1)
        ('b', '4') -> (1,0)
        ('c', '1') -> (2,3)
        ('c', '2') -> (2,2)
        ('c', '3') -> (2,1)
        ('c', '4') -> (2,0)
        ('d', '1') -> (3,3)
        ('d', '2') -> (3,2)
        ('d', '3') -> (3,1)
        ('d', '4') -> (3,0)
        _ -> error "Invalid Coord"


{- if the coord parser was not called from the parserForInput, parserForDrawLose must be called. since the only other allowed charcters are hash symbol and d, 
  if either evalutes to true, their corresponding parser is called, using the choice combinator.
  If the input being parsed starts with the '#' character, then the hash parser is used, which matches the '#' character and returns the tuple (40, 40). 
  If the input starts with the 'D' character, then the dSymbol parser is used, which matches the 'D' character and returns the tuple (30, 30). 
  If the input starts with any other character, the parser fails and returns an error. -}

parserForDrawLose :: Parser (Int, Int)
parserForDrawLose = choice [hash, dSymbol]


-- parser for the # symbol
hash :: Parser (Int, Int)
hash = do
  _ <- char '#'
  return (40, 40)

--parser for the Draw symbol
dSymbol :: Parser (Int, Int)
dSymbol = do
  _ <- char 'D'
  return (30, 30)








{- takes a Board and a list of (Int, Int) tuples representing moves made by the players. If the list contains only one tuple and that tuple is (30, 30), 
it checks if the game is drawn and prints the result accordingly. If the game is drawn, it returns the original Board. 
  If the list contains only one tuple and that tuple is (40, 40), it checks if the game is won by either the blue or red player, prints the result accordingly, 
    and returns the original Board. If the list contains two tuples, it tries to make a move on the Board with the makeMove function using the from and to tuples 
    as the source and destination coordinates. If the move is valid, it shows the resulting board and returns the updated Board. Otherwise, it returns an error message.
Otherwise, it returns an error message, since that would be invalid input. -}

produceNewBoard :: Board -> [(Int, Int)] -> IO Board
produceNewBoard board [(30, 30)]
  | gameIsDrawn [board] = do
      showBoard board
      putStrLn "The game is drawn."
      return board
  | otherwise = fail "Invalid file"
produceNewBoard board [(40, 40)]
  | gameIsWon board BluePlayer = do
      showBoard board
      putStrLn "The Blue player wins!"
      return board
  | gameIsWon board RedPlayer = do
      showBoard board
      putStrLn "The Red player wins!"
      return board
  | otherwise = fail "Invalid file"
produceNewBoard board [from, to] =
  case makeMove board from to of
    Just recentBoard -> do
      showBoard recentBoard
      return recentBoard
    Nothing -> fail "Invalid file"
produceNewBoard _ _ = fail "Invalid file"


--show board is a function that uses the show instance of board to print the board
showBoard :: Board -> IO ()
showBoard board = do
  putStrLn "Current state of board:"
  putStrLn $ show board



-- newtype Parser a = Parser {runParser :: String ->  Maybe(a,String)}
--function that takes a string as input. it can fail so maybe type. parsers dont have to consume everything. although we return the structured output, we also return the stuff that wasn't parsed. 

-- instance Functor Parser where
--     fmap f (Parser z) = Parser $ \s -> do
--      (z', s') <- z s --running the parser with the input 
--      return (f z', s')

-- instance Applicative Parser where
--     pure z = Parser $ \s -> Just (z,s) --takes a string as an input and i am returning z and that string

--     (Parser f) <*> (Parser x) = Parser $ \s -> do
--         (f', s1) <- f s
--         (x', s2) <- x s1
--         return (f' x', s2)
-- returns what it parsed f' x' along with anything it didnt parse s2. 

-- instance Monad Parser where
--   (Parser x) >>= f = Parser $ \s -> do
--     (x', s') <- x s
--     Main.runParser (f x') s's

-- instance MonadFail Parser where
--     fail _ = Parser $ \s -> Nothing

-- class (Applicative f) => Alternative f where
--     empty :: f a
--     ( <|> ) :: f a -> f a -> f a
--     some :: f a -> f [a]
--     some q = some_q
--         where
--             many_q = some_q Main.<|> pure []
--             some_q = (:) <$> q <*> many_q
--     many :: f a -> f [a]
--     many q = many_q
--         where
--             many_q = some_q Main.<|> pure []
--             some_q = (:) <$> q <*> many_q
-- --constructing a list within a applicative functor. return 


-- instance Main.Alternative Parser where
--     empty = fail ""
--     (Parser x ) <|> (Parser y) = Parser $ \s ->
--         case x s of
--             Just x -> Just x
--             Nothing -> y s

--got 2 parsers if 1 succeds, we return the output
--if it fails, run the 2nd parser 

stringToCoord s = case parse coord "" s of
  Left _ -> error "Invalid coordinate format"
  Right c -> c

-- Use the coord parser in the cellToPosition function
cellToPosition :: String -> String
cellToPosition "#" = "lost"
cellToPosition "D" = "game drawn"
cellToPosition cell
  | length cell /= 3 || head cell /= '(' || last cell /= ')' = error $ "Error parsing cell: invalid format: " ++ cell
  | otherwise = case parse coord "" (init (tail cell)) of
      Left err -> error $ "Error parsing cell: " ++ show err
      Right pos -> show pos
