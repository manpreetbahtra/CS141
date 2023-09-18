module Main where

import System.Random
import Text.Read

main :: IO ()
-- main = print "Not implemented yet :)"
main = do 
    -- getRand >>= print 
    getRand
    print "Thinking of a number"
    i <- getRand
    x <- getLine
    let 
        q = check (read x)  i 
    print q
    play i 1
    main 




echo :: IO()
echo = do
    x <- getLine
    putStrLn x
    echo 

getRand :: IO Int
getRand = randomRIO (1,100)

check :: Int -> Int -> Maybe String
check x i = do 
    if x < i then Just "higher!" else (if x > i then Just "Lower!" else Nothing)

play :: Int -> Int -> IO()
play answer guesses = do
    cc <- getLine
    let 
        result = readMaybe cc 
        -- if anything in a do notation isnt an IO operattion use a let. 
    case result of 
        Nothing -> do
            putStrLn "That's not a number"
            play answer guesses
        Just a -> 
            case check a answer of 
                Nothing -> putStrLn  $ "You win! Got it in " ++ show guesses
                Just some -> do 
                    -- print $ check answer a 
                    putStrLn some 
                    play answer $ guesses+1


    