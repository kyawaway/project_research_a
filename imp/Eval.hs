module Eval where

import Syntax
import Parser 


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
