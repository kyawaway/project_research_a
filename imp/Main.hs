module Main where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Map

import Parser
import Eval 

{-
-- repl 

flushSrt :: String -> IO ()
flushSrt str = putStr str >> hFlush stdout 

readPrompt :: String -> IO String 
readPrompt prompt = flushSrt prompt >> getLine 

readExpr :: String -> Command
readExpr s = case parse parseCommand "stdin" s of
--            Left err -> 
            Right x -> x

readEvalPrint :: String -> IO ()
readEvalPrint = putStrLn . show . evalCommand Data.Map.empty readExpr
                    
loopUntil :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
loopUntil pred prompt action = do {
    x <- prompt;
    if pred x 
        then return ()
        else (action x >> loopUntil pred prompt action)
               



-- main

main :: IO ()
main = do putStrLn "Enter expression:"
          loopUntil (== "quid") (readPrompt ">>") readEvalPrint               
-}


main :: IO ()
main = do putStrLn "Enter expression:"
          s <- getLine
          case parse parseWhileProgram "stdin" s of
            Left err -> print err
            Right x -> do print x
                          print (evalCommand  Data.Map.empty x)

