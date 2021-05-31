module Syntax where 

    
data Command = Seq [Command]
          | If BExpr Command Command
          | While BExpr Command 
          | Assign String AExpr
          | Skip
          deriving (Eq,Show)

-- Assign の String は Parser.parseAssign で 
-- identifier として受け取るため
-- Assign Id AExpr みたいな扱い

data BExpr = Bool Bool
          |  Greater AExpr AExpr
          |  Less AExpr AExpr
          deriving (Eq,Show)

-- <greater|less> :: AExpr, AExpr -> Bool

data AExpr = Id String 
          | Integer Integer
          | Add AExpr AExpr
          | Sub AExpr AExpr
          | Mul AExpr AExpr
          | Div AExpr AExpr
          | Pow AExpr AExpr
          | Fact AExpr
          | Negate AExpr
          deriving (Eq, Show)
           
