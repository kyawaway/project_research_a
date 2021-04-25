import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as TT
import qualified Text.Parsec.Language as Lang

type Parser a = Parsec String () a


-- ast 
data Expr = Const Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Fact Expr
          | Negate Expr
          deriving (Eq, Show)
-- lexer

symbol :: String -> Parser String
symbol = TT.symbol Lang.haskell


reservedOp :: String -> Parser()
reservedOp = TT.reservedOp Lang.haskell


natural :: Parser Integer
natural = TT.natural Lang.haskell

whiteSpace :: Parser ()
whiteSpace = TT.whiteSpace Lang.haskell

-- parse

--  ( exp ) | num
atom :: Parser Expr
atom = do symbol "("
          x <- expr
          symbol ")"
          return x
   <|> (Const <$> natural)



-- evaluate whole expressions 
wholeExpr :: Parser Expr
wholeExpr = do whiteSpace
               x <- expr
               eof
               return x


-- operator
expr :: Parser Expr
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
eval :: Expr -> Integer
eval (Const x) = x
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

