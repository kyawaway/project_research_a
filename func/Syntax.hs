module Syntax where

data Statement = Seq [Statement]
          | If Expr Statement Statement
          | While Expr Statement  
          | Assign String Expr
          | Return Expr
          | Print Expr
          | Skip
          deriving (Eq,Show)


data Expr =  Var String   
          |  Integer Integer 
          |  Bool Bool
          |  Func String Statement 
          |  Apply Expr Expr
          |  Negative Expr
          |  Pow Expr Expr
          |  Mul Expr Expr
          |  Div Expr Expr
          |  Add Expr Expr
          |  Sub Expr Expr
          |  Greater Expr Expr
          |  Less Expr Expr
          |  Equal Expr Expr
          deriving (Eq,Show)

-- data Op

