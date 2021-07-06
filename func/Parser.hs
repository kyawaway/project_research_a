module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax

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
        ,"return"
        ,"func"
        ,"print"
    ]
    ,Token.reservedOpNames = ["+", "-", "*", "/", ":=", "<", ">","=="]
    }

lexer = Token.makeTokenParser reserveWord

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer

-- ParseExpr 

parseExpr:: Parser Expr
parseExpr = buildExpressionParser
       [[binary "^" Pow AssocRight]
       ,[binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
       ,[binary "+" Add AssocLeft, binary "-" Sub AssocLeft, prefix "-" Negative]
       ,[binary ">" Greater AssocLeft, binary "<" Less AssocLeft, binary "==" Equal AssocLeft]
       ]
       exprTerm 
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)


exprTerm :: Parser Expr
exprTerm = parens parseExpr
        <|> Integer <$> integer
        <|> parseBool
        <|> Var <$> identifier

---- Bool

parseBool :: Parser Expr
parseBool = try parseTrue <|> try parseFalse

parseTrue :: Parser Expr
parseTrue = string "true" >> return (Bool True)

parseFalse :: Parser Expr
parseFalse = string "false" >> return (Bool False)

---- function

parseFunc :: Parser Expr 
parseFunc = do reserved "func"
               args <- parens parseExpr 
               stmt <- braces parseStatement
               return $ Func args stmt 

---- apply

parseApply :: Parser Expr
parseApply = do func <- parseExpr 
                param <- parens parseExpr 
                return $ Apply func param 

-- Parse Statement 

parseStatement :: Parser Statement
parseStatement = parens parseStatement
   <|> sequenceOfStatement


sequenceOfStatement  =
    do list <- sepBy1 statement semi 
       return $ if length list == 1 then head list else Seq list


statement  :: Parser Statement
statement  = parseSkipStatement
        <|> parseIfStatement
        <|> parseWhileStatement
        <|> parseAssignStatement
        <|> parseReturnStatement

---- skip 
parseSkipStatement  :: Parser Statement
parseSkipStatement  = reserved "skip" >> return Skip 

---- if 

parseIfStatement  :: Parser Statement
parseIfStatement  =
    do reserved "if"
       cond <- parens parseExpr  
       reserved "then"
       stmt1 <- braces parseStatement
       reserved "else"
       If cond stmt1 <$> braces parseStatement


---- while

parseWhileStatement  :: Parser Statement
parseWhileStatement  =
    do reserved "while"
       cond <-  parens parseExpr
       reserved "do"
       stmt <-  braces parseStatement
       return $ While cond stmt 


---- assign

parseAssignStatement  :: Parser Statement
parseAssignStatement  =
    do var <- identifier
       reservedOp ":="
       Assign var <$> parseExpr

---- return 

parseReturnStatement :: Parser Statement 
parseReturnStatement = 
        do reserved "return"
           Return <$> parseExpr


-- parse Program

parseWhileProgram :: Parser Statement
parseWhileProgram = whiteSpace >> parseStatement
