import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as TT
import qualified Text.Parsec.Language as Lang

type Parser a = Parsec String () a

symbol :: String -> Parser String
symbol = TT.symbol Lang.haskell


reservedOp :: String -> Parser()
reservedOp = TT.reservedOp Lang.haskell


natural :: Parser Integer
natural = TT.natural Lang.haskell

atom :: Parser Integer
atom = do symbol "("
          x <- expr
          symbol ")"
          return x
   <|> natural

expr :: Parser Integer
expr = buildExpressionParser
        [[binary "*" (*) AssocLeft, binary "/" div AssocLeft]
        ,[binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
        ]
        atom
    where
      binary name fun assoc = Infix (reservedOp name >> return fun) assoc


main :: IO ()
main = do putStrLn "Enter expression:"
          s <- getLine
          case parse expr "stdin" s of
            Left err -> print err
            Right x -> print x

