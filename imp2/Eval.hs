module Eval where

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
                                                     TypeBool True ->  evalStatement env (Seq[s,While e s]) 
                                                     _ -> env

evalStatement env (Assign x n) = setVal env x (evalExpr env n)   
evalStatement env Skip = env 

evalExpr :: Env -> Expr -> TypeEnv  

-- Bool

evalExpr env (Bool True) = TypeBool True
evalExpr env (Bool False) = TypeBool False 

evalExpr env (Greater  x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                 TypeInteger b -> TypeBool (a > b)
                                _ -> error "TypeError in '>' "

evalExpr env (Less  x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                 TypeInteger b -> TypeBool (a < b)
                                _ -> error "TypeError in '<' "


evalExpr env (Equal x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                   TypeInteger b -> TypeBool (a == b)
                                _ -> error "TypeError in '==' "

-- Integer 

evalExpr env (Integer x) = TypeInteger x

evalExpr env (Add  x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                 TypeInteger b -> TypeInteger  (a + b)
                                _ -> error "TypeError in '+' "

evalExpr env (Sub  x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                 TypeInteger b -> TypeInteger  (a - b)
                                _ -> error "TypeError in '-' "

evalExpr env (Mul  x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                 TypeInteger b -> TypeInteger  (a * b)
                                _ -> error "TypeError in '*' "

evalExpr env (Div  x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                 TypeInteger b -> TypeInteger  (div a b)
                                _ -> error "TypeError in '/' "

evalExpr env (Pow  x y) = case evalExpr env x of 
                                TypeInteger a -> case evalExpr env y of 
                                                 TypeInteger b -> TypeInteger  (a ^ b)
                                _ -> error "TypeError in '^' "

evalExpr env (Negative x) = case evalExpr env x of 
                              TypeInteger a -> TypeInteger (-a)
                              _ -> error "TypeError in '-' "

evalExpr env (Var  x) = test $ getVal env x




