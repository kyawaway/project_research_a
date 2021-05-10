import System.Environment
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as TT
import qualified Text.Parsec.Language as Lang

type Parser a = Parsec String () a


-- ast 


data Stmt = If BExpr Stmt Stmt 
          | Assign String AExpr 
          | Skip 



data BExpr = Bool Bool 
          |  Greater AExpr AExpr


data AExpr = Var String  
          | Integer Integer
          | Add AExpr AExpr
          | Sub AExpr AExpr
          | Mul AExpr AExpr
          | Div AExpr AExpr
          | Pow AExpr AExpr
          | Fact AExpr
          | Negate AExpr
          deriving (Eq, Show)
-- lexer

symbol :: String -> Parser String
symbol = TT.symbol Lang.haskell

reserved :: String -> Parser()
reserved = TT.reserved Lang.haskell 

reservedOp :: String -> Parser()
reservedOp = TT.reservedOp Lang.haskell


natural :: Parser Integer
natural = TT.natural Lang.haskell

whiteSpace :: Parser ()
whiteSpace = TT.whiteSpace Lang.haskell



-- parse

--  ( exp ) | num
atom :: Parser AExpr
atom = do symbol "("
          x <- expr
          symbol ")"
          return x
   <|> (Integer <$> natural)

-- BExpr

parseBExpr :: Parser BExpr 
parseBExpr = parseBool 


parseBool :: Parser BExpr
parseBool = try parseTrue <|> try parseFalse

parseTrue :: Parser BExpr
parseTrue = string "true" >> return (Bool True)

parseFalse :: Parser BExpr
parseFalse = string "false" >> return (Bool False)

-- stmt

--if

{-
parseStmt :: Parser Stmt
parseStmt = parseIf


parseIf :: Parser Stmt
parseIf = do reserved "if"
             cond <- parseBExpr
             reserved "then"
             stmt1 <- parseStmt 
             reserved "else"
             stmt2 <- parseStmt
             return $ if cond stmt1 stmt2 

-}


-- evaluate whole expressions 

wholeExpr :: Parser AExpr
wholeExpr = do whiteSpace
               x <- expr
               eof
               return x


-- operator
expr :: Parser AExpr
expr = buildExpressionParser
       [[binary "^" Pow AssocRight]
       ,[binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
       ,[binary "+" Add AssocLeft, binary "-" Sub AssocLeft, prefix "-" Negate]
       ]
       atom
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)

-- evaluate ast
eval :: AExpr -> Integer
eval (Integer x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Pow x y) = eval x ^ eval y
eval (Negate x) = - eval x


main :: IO ()
main = do putStrLn "Enter expression:"
          s <- getLine
          case parse expr "stdin" s of
            Left err -> print err
            Right x -> do print x
                          print (eval x)

          