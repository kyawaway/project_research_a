import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- ast 

-- data Stmt 

data Command = Seq [Command]
          | If BExpr Command Command
          | While BExpr Command 
          | Assign String AExpr
          | Skip
          | BExpr BExpr
          | AExpr AExpr
          deriving (Eq,Show)



data BExpr = Bool Bool
          |  Greater AExpr AExpr
          |  Less AExpr AExpr
          deriving (Eq,Show)

-- <greater|less> :: AExpr, AExpr -> Bool

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
        ,"while"
        ,"do"
        ,"else"
        ,"skip"
        ,"true"
        ,"false"
    ]
    ,Token.reservedOpNames = ["+", "-", "*", "/", ":=", "<", ">"]
    }

lexer = Token.makeTokenParser reserveWord

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer


--parser


-- parse AExpr

parseAExpr :: Parser AExpr
parseAExpr = buildExpressionParser
       [[binary "^" Pow AssocRight]
       ,[binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
       ,[binary "+" Add AssocLeft, binary "-" Sub AssocLeft, prefix "-" Negate]
       ]
       aTerm
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)

aTerm :: Parser AExpr
aTerm = parens parseAExpr
    <|> Integer <$> integer



-- parse BExpr

parseBExpr :: Parser BExpr
parseBExpr = parseBool
    <|> parseRelation

parseBool :: Parser BExpr
parseBool = try parseTrue <|> try parseFalse

parseTrue :: Parser BExpr
parseTrue = string "true" >> return (Bool True)

parseFalse :: Parser BExpr
parseFalse = string "false" >> return (Bool False)

parseRelation =
    do a1 <- parseAExpr
       relationOp <- makeRelationOp
       a2 <- parseAExpr
       return $ relationOp a1 a2

makeRelationOp = (reservedOp ">" >> return Greater)
       <|> (reservedOp "<" >> return Less) 

-- parse Command

parseWhile :: Parser Command
parseWhile = whiteSpace >> command

command :: Parser Command
command = parens command
   <|> sequenceOfCommand

sequenceOfCommand =
    do list <- sepBy1 parseCommand semi
       return $ if length list == 1 then head list else Seq list


parseCommand :: Parser Command
parseCommand = parseIfCommand
        <|> parseWhileCommand
        <|> parseAssignCommand
        <|> parseAExprCommand
        <|> parseBExprCommand

-- if 

parseIfCommand :: Parser Command
parseIfCommand =
    do reserved "if"
       cond <- parseBExpr 
       reserved "then"
       stmt1 <- parseCommand
       reserved "else"
       If cond stmt1 <$> parseCommand

-- while 
parseWhileCommand :: Parser Command
parseWhileCommand = 
    do reserved "while"
       cond <- parseBExpr
       reserved "do"
       stmt <- parseCommand
       return $ While cond stmt


-- assign

parseAssignCommand :: Parser Command
parseAssignCommand = 
    do var <- identifier
       reservedOp ":="
       expr <- parseAExpr
       return $ Assign var expr

parseBExprCommand :: Parser Command
parseBExprCommand = 
    do expr <- parseBExpr
       return $ BExpr expr 

parseAExprCommand :: Parser Command
parseAExprCommand = 
    do expr <- parseAExpr
       return $ AExpr expr 

-- evaluate



-- evaluate ast

evalCommand (If b x y ) = if evalBExpr b then evalCommand x else evalCommand y
evalCommand (AExpr x) = evalAExpr x
--evalCommand (BExpr b) = evalBExpr b


evalBExpr :: BExpr -> Bool 
evalBExpr (Bool True) = True
evalBExpr (Bool False) = False 
evalBExpr (Greater x y) = evalAExpr x > evalAExpr y
evalBExpr (Less x y) = evalAExpr x < evalAExpr y



evalAExpr :: AExpr -> Integer
evalAExpr (Integer x) = x
evalAExpr (Add x y) = evalAExpr x + evalAExpr y
evalAExpr (Sub x y) = evalAExpr x - evalAExpr y
evalAExpr (Mul x y) = evalAExpr x * evalAExpr y
evalAExpr (Div x y) = evalAExpr x `div` evalAExpr y
evalAExpr (Pow x y) = evalAExpr x ^ evalAExpr y
evalAExpr (Negate x) = - evalAExpr x


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
readEvalPrint = putStrLn . show . evalCommand . readExpr
                    
loopUntil :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
loopUntil pred prompt action = do {
    x <- prompt;
    if pred x 
        then return ()
        else (action x >> loopUntil pred prompt action)
}               



-- main

main :: IO ()
main = do putStrLn "Enter expression:"
          loopUntil (== "quid") (readPrompt ">>") readEvalPrint               
-}

main :: IO ()
main = do putStrLn "Enter expression:"
          s <- getLine
          case parse parseCommand "stdin" s of
            Left err -> print err
            Right x -> do print x
                          print (evalCommand x)
