
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- ast 
data Stmt = If BExpr Stmt Stmt 
          | Assign String AExpr 
          | Skip 
          deriving (Eq,Show)



data BExpr = Bool Bool 
          |  Greater AExpr AExpr
          deriving (Eq,Show)


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

reserveWord = 
    emptyDef { Token.reservedNames = 
        ["if"
        ,"then"
        ,"else"
        ,"skip"
        ,"true"
        ,"false"
    ]
    ,Token.reservedOpNames = ["+", "-", "*", "/", ":=", "<", ">"]
    }

lexer = Token.makeTokenParser reserveWord

idetifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer


-- parse AExpr

atom :: Parser AExpr
atom = do 
    parens expr 
    <|> (Integer <$> integer)

-- parse BExpr

parseBExpr :: Parser BExpr 
parseBExpr = parseBool 


parseBool :: Parser BExpr
parseBool = try parseTrue <|> try parseFalse

parseTrue :: Parser BExpr
parseTrue = string "true" >> return (Bool True)

parseFalse :: Parser BExpr
parseFalse = string "false" >> return (Bool False)


-- parse Stmt
parseWhile :: Parser Stmt 
parseWhile = whiteSpace >> stmt 

stmt :: Parser Stmt 
stmt = parens stmt 
   <|> sequenceOfStmt

sequenceOfStmt = do list 

parseStmt :: Parser Stmt 
parseStmt = parseIf 


-- if 

parseIf :: Parser Stmt
parseIf = do reserved "if"
             cond <- parseBExpr
             reserved "then"
             stmt1 <- parseStmt 
             reserved "else"
             stmt2 <- parseStmt
             return $ if cond stmt1 stmt2 





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

