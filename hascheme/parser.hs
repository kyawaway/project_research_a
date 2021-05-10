module Main where
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


-- data type

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool




showVal :: LispVal -> String
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f" 

---- helper

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal



symbol :: Parser Char
symbol = oneOf "!#$%&|+-*/:<>=@?^_"


spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read



parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
  

-- REPL

-- read

readExpr :: String -> LispVal
readExpr input = case (parse parseExpr "lisp" input) of
  Left err -> Atom $ "No match: " ++ show err
  Right val ->  val


-- eval 

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '(' 
               x <- try parseList <|> parseDottedList
               char ')' 
               return x



eval :: LispVal -> LispVal
eval lispVal = lispVal

readPrompt :: String -> IO String
readPrompt  prompt = putStr prompt >> hFlush stdout >> getLine


readEvalPrint :: String -> IO()
readEvalPrint = putStrLn . show . eval . readExpr

loopUntil :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
loopUntil pred prompt action = do {
  x <- prompt;
  if pred x
     then return ()
     else (action x >> loopUntil pred prompt action)

}


main :: IO ()
main = loopUntil (== "quit") (readPrompt ">>") readEvalPrint
  


