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
evalStatement env (Seq []) = env 
evalStatement env (Seq (h:t)) = let new_env = evalStatement env h in evalStatement new_env (Seq t)
 
evalStatement env (If b x y) = case evalExpr env b of 
                                                TypeBool True -> evalStatement env x
                                                TypeBool False -> evalStatement env y
                                                _ -> error "If statement expected type 'Bool' as Expr"

evalStatement env (While e s) = case evalExpr env e of 
                                                     TypeBool True ->  evalStatement env (Seq[s,(While e s)]) 
                                                     _ -> env

evalStatement env (Assign x n) = setVal env x (evalExpr env n)   
evalStatement env Skip = env 

evalExpr :: Env -> Expr -> TypeEnv  
evalExpr env (Bool True) = TypeBool True
evalExpr env (Bool False) = TypeBool False 

-- TypeEnv x = x みたいな関数が欲しいど返り値の型が複数種類(Integer, Bool)になる

{-
evalExpr env (Greater x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeBool (x > y) else error "hoge"
evalExpr env (Less x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeBool (x < y) else error "hoge"
evalExpr env (Equal x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeBool (x == y) else error "hoge"
-}
evalExpr env (Integer x) = TypeInteger x

{-
evalExpr env (Add x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeInteger (x + y) else error "hoge"
evalExpr env (Sub x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeInteger (x - y) else error "hoge"
evalExpr env (Mul x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeInteger (x * y) else error "hoge"
evalExpr env (Div x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeInteger (div x y) else error "hoge"
evalExpr env (Pow x y) = if (evalExpr env x == TypeInteger x) && (evalExpr env y == TypeInteger y) then TypeInteger (x ^ y) else error "hoge"
evalExpr env (Negative x) = if evalExpr env x == TypeInteger x then TypeInteger (-x) else error "hoge"
-}

evalExpr env (Var  x) = test $ getVal env x
evalExpr env _ = error "hoge"
