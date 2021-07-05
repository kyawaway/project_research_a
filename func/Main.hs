module Main where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Map
import Data.IORef
import Parser
import Env
import Eval

{-

main :: IO ()
main = do putStrLn "Enter expression:"
          s <- getLine
          case parse parseWhileProgram "stdin" s of
            Left err -> print err
            Right x -> do print x
                          evalStatement Nullenv x
-}


main :: IO ()
main = do
    env <- nullEnv
    loop env
  where
    loop env = do putStr ">> "
                  hFlush stdout
                  s <- getLine
                  case parse parseWhileProgram "stdin" s of
                      Left err -> print err >> loop env
                      Right x -> do print x
                                    a <- evalStatement env x
                                    print a
                                    loop env


{-
readPrompt :: String -> IO String 
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

readEval :: Env -> String -> IO ()
readEval env x = case parse parseWhileProgram "stdin" x of 
                    Right ast -> do a <- evalStatement env ast
                                    print a



loopUntil :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
loopUntil pred prompt action = do {
    input <- prompt;
    if pred input 
      then return ()
      else (action input >> loopUntil pred prompt action)
}

loop :: IO ()
loop = nullEnv >>= loopUntil (== "quit") (readPrompt ">> ") . readEval

main :: IO ()
main = loop

-}


