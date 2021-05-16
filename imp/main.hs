

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
          | Assign String AExpr
          | Skip
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

-- parse Stmt

parseWhile :: Parser Command
parseWhile = whiteSpace >> command

command :: Parser Command
command = parens command
   <|> sequenceOfCommand

sequenceOfCommand =
    do list <- sepBy1 parseCommand semi
       return $ if length list == 1 then head list else Seq list


parseCommand :: Parser Command
parseCommand = parseIf
        <|> parseAssign


-- if 

parseIf :: Parser Command
parseIf =
    do reserved "if"
       cond <- parseBExpr 
       reserved "then"
       stmt1 <- parseCommand
       reserved "else"
       If cond stmt1 <$> parseCommand

-- assign

parseAssign :: Parser Command
parseAssign = 
    do var <- identifier
       reservedOp ":="
       expr <- parseAExpr
       return $ Assign var expr



-- evaluate



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
          case parse parseCommand "stdin" s of
            Left err -> print err
            Right x -> do print x
                         

