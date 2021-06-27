module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
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
    ]
    ,Token.reservedOpNames = ["+", "-", "*", "/", ":=", "<", ">"]
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
    <|> Var <$> identifier

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

command :: Parser Command
command = parens command
   <|> sequenceOfCommand

sequenceOfCommand =
    do list <- sepBy1 parseCommand semi
       return $ if length list == 1 then head list else Seq list


parseCommand :: Parser Command
parseCommand = parseSkipCommand 
        <|> parseIfCommand
        <|> parseWhileCommand
        <|> parseAssignCommand

-- skip 
parseSkipCommand :: Parser Command 
parseSkipCommand = reserved "skip" >> return Skip 

-- if 

parseIfCommand :: Parser Command
parseIfCommand =
    do reserved "if"
       cond <- parens parseBExpr 
       reserved "then"
       stmt1 <- braces parseCommand
       reserved "else"
       If cond stmt1 <$> braces parseCommand

-- while 
parseWhileCommand :: Parser Command
parseWhileCommand = 
    do reserved "while"
       cond <- parens parseBExpr
       reserved "do"
       stmt <- braces command
       return $ While cond stmt


-- assign

parseAssignCommand :: Parser Command
parseAssignCommand = 
    do var <- identifier
       reservedOp ":="
       expr <- parseAExpr
       return $ Assign var expr


-- Parse Program
parseWhileProgram :: Parser Command
parseWhileProgram = whiteSpace >> command
