module Eval where

import Parser 

import Control.Exception
import Control.Monad.Except 
import Data.Typeable
import Data.Map.Strict
import Prelude hiding(lookup)

import Syntax
import Env 


-- evaluate ast

evalCommand env (Seq []) = env 
evalCommand env (Seq (h:t)) = let new_env = evalCommand env h in evalCommand new_env (Seq t)
 
evalCommand env Skip = env 
evalCommand env (If b x y) = if evalBExpr env b then evalCommand env x else evalCommand env y
evalCommand env (Assign x n) = setVal env x (evalAExpr env n)   
evalCommand env (While e s) = if evalBExpr env e then evalCommand env (Seq[s,While e s]) else env 

evalBExpr :: Env -> BExpr -> Bool 
evalBExpr env (Bool True) = True
evalBExpr env (Bool False) = False 
evalBExpr env (Greater x y) = evalAExpr env x > evalAExpr env y
evalBExpr env (Less x y) = evalAExpr env x < evalAExpr env y



evalAExpr :: Env -> AExpr -> Integer
evalAExpr env (Integer x) = x
evalAExpr env (Add x y) = evalAExpr env x + evalAExpr env y
evalAExpr env (Sub x y) = evalAExpr env  x - evalAExpr env y
evalAExpr env (Mul x y) = evalAExpr env x * evalAExpr env y
evalAExpr env (Div x y) = evalAExpr env x `div` evalAExpr env y
evalAExpr env (Pow x y) = evalAExpr env x ^ evalAExpr env y
evalAExpr env (Negate x) = - evalAExpr env x
evalAExpr env (Var x) = test $ getVal env x 
