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
       exprTerm <* semi
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)

-- セミコロンまわり微妙(後述)

exprTerm :: Parser Expr
exprTerm = parens parseExpr
        <|> Integer <$> integer
        <|> parseBool
        <|> Var <$> identifier

--- Bool

parseBool :: Parser Expr
parseBool = try parseTrue <|> try parseFalse

parseTrue :: Parser Expr
parseTrue = string "true" >> return (Bool True)

parseFalse :: Parser Expr
parseFalse = string "false" >> return (Bool False)


-- Parse Statement 

statement :: Parser Statement
statement = parens statement
   <|> sequenceOfStatement


sequenceOfStatement  =
    do list <- many1 parseStatement 
       return $ if length list == 1 then head list else Seq list



parseStatement  :: Parser Statement
parseStatement  = parseSkipStatement
        <|> parseIfStatement
        <|> parseWhileStatement
        <|> parseAssignStatement

-- skip 
parseSkipStatement  :: Parser Statement
parseSkipStatement  = reserved "skip" <* semi  >> return Skip 

-- if 

parseIfStatement  :: Parser Statement
parseIfStatement  =
    do reserved "if"
       cond <- parens parseExpr 
       reserved "then"
       stmt1 <- braces statement
       reserved "else"
       If cond stmt1 <$> braces statement <* semi

-- cond <- parens parseExpr <* semi  にすると (expr); みたいな形になる　
-- (expr;) にしたいから，とりあえずparseExprのほうでsemi受理させて，その代わりAssignでsemiを受理しないみたいな格好になっている

-- do symbol "("
--          cond <- parseExpr <* semi
--          symbol ")"
--          return cond
--
-- みたいにやれば解決するけど...

-- while 
parseWhileStatement  :: Parser Statement
parseWhileStatement  =
    do reserved "while"
       cond <-  parens parseExpr 
       reserved "do"
       stmt <-  braces statement <* semi
       return $ While cond stmt 


-- assign

parseAssignStatement  :: Parser Statement
parseAssignStatement  =
    do var <- identifier
       reservedOp ":="
       Assign var <$> parseExpr


-- Parse Program
parseWhileProgram :: Parser Statement
parseWhileProgram = whiteSpace >> statement
