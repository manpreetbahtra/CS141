module Main (main) where

import Parsing
import Text.Megaparsec

main :: IO ()
-- main = error "Start with src/Parsing.hs!"
main = do
    line <- getLine 
    let result = parse parseOp "" line 
    case result of 
        Left err -> putStrLn $ errorBundlePretty err 
        Right expr -> print $ eval expr
   