
module Main where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Map

import Parser


main :: IO ()
main = do putStrLn "Enter expression:"
          s <- getLine
          case parse parseWhileProgram "stdin" s of
            Left err -> print err
            Right x -> do print x

