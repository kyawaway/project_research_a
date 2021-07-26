module Eval where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Typeable

import Data.Map.Strict
import Prelude hiding(lookup)

import Syntax
import Env 


-- evaluate ast

-- Statement

evalStatement :: Env -> Statement -> IO (Maybe TypeEnv)  

evalStatement env (Seq []) = return Nothing   

evalStatement env (Seq (h:t)) = do car <- evalStatement env h
                                   maybe (evalStatement env (Seq t)) (return . Just) car
 

evalStatement env (If b x y) = do
    cond <- evalExpr env b
    case cond of
        TypeBool True -> evalStatement env x
        TypeBool False -> evalStatement env y
        _ -> error "If statement expected type 'Bool' as Expr"

evalStatement env (While e s) = do
    cond <- evalExpr env e
    case cond of 
        TypeBool True ->  evalStatement env (Seq[s,While e s]) 
        _ -> return Nothing


evalStatement env (Assign x n) = do 
    val <- evalExpr env n 
    defineVar env x val >> return Nothing


evalStatement env Skip = return Nothing  

evalStatement env (Return x) = Just <$> evalExpr env x


-- Expr 

evalExpr :: Env -> Expr -> IO TypeEnv  

---- Bool

evalExpr env (Bool True) = return (TypeBool True)
evalExpr env (Bool False) = return (TypeBool False) 


---- Integer 

evalExpr env (Integer x) = return (TypeInteger x)


---- Val 

evalExpr env (Var  x) = getVal env x

---- Func 

evalExpr env (Func arg body) =  return (Closure env arg body)

---- Apply

evalExpr env (Apply funcname param) = do 
        func <- evalExpr env funcname
        case func of 
                  Closure closureEnv arg body -> do
                      value <- evalExpr env param 
                      newenv <- push closureEnv arg value
                      result <- evalStatement newenv body
                      garbage <- pop newenv
                      maybe (return Null) return result
                  _ -> error "Error in func"

------ BoolOp

evalExpr env (Greater x y) = do val1 <- evalExpr env x 
                                val2 <- evalExpr env y
                                return (evalOp2Bool val1 val2 (>)) 

evalExpr env (Less x y) = do val1 <- evalExpr env x 
                             val2 <- evalExpr env y
                             return (evalOp2Bool val1 val2 (<)) 

evalExpr env (Equal x y) = do val1 <- evalExpr env x 
                              val2 <- evalExpr env y
                              return (evalOp2Bool val1 val2 (==)) 

---- IntegerOp

evalExpr env (Add x y) = do val1 <- evalExpr env x 
                            val2 <- evalExpr env y
                            return (evalOp2Integer val1 val2 (+)) 

evalExpr env (Sub x y) = do val1 <- evalExpr env x 
                            val2 <- evalExpr env y
                            return (evalOp2Integer val1 val2 (-)) 

evalExpr env (Mul x y) = do val1 <- evalExpr env x 
                            val2 <- evalExpr env y
                            return (evalOp2Integer val1 val2 (*)) 


evalExpr env (Div x y) = do val1 <- evalExpr env x 
                            val2 <- evalExpr env y
                            return (evalOp2Integer val1 val2 div) 

evalExpr env (Pow x y) = do val1 <- evalExpr env x 
                            val2 <- evalExpr env y
                            return (evalOp2Integer val1 val2 (^)) 
 
evalExpr env (Negative x) = do val <- evalExpr env x
                               return (evalOp1 val negate)

-- binary Op

---- BoolOp

evalOp2Bool :: TypeEnv -> TypeEnv -> (Integer -> Integer -> Bool) -> TypeEnv
evalOp2Bool (TypeInteger x) (TypeInteger y) func = TypeBool (func x y)
evalOp2Bool _ _ _ = error "TypeError"


---- IntegerOp

evalOp2Integer :: TypeEnv -> TypeEnv -> (Integer -> Integer -> Integer) -> TypeEnv
evalOp2Integer (TypeInteger x) (TypeInteger y) func = TypeInteger (func x y)
evalOp2Integer _ _ _ = error "TypeError"


---- ほんとはもっと汎用がいい
---- 型に対するパータンマッチの書き方がよくわかってない

{-
evalOp2 :: TypeEnv -> TypeEnv -> (Integer -> Integer -> *) -> TypeEnv
evalOp2 (TypeInteger x) (TypeInteger y) func = case func x y of
                                                   Integer a -> TypeInteger a
                                                   Bool a -> TypeBool a
                                                   _ -> error "Error"

evalOp2 _ _ _ = error "Error"
-}


-- unary Op

evalOp1 :: TypeEnv -> (Integer -> Integer) -> TypeEnv
evalOp1 (TypeInteger x) func = TypeInteger (func x)
evalOp1 _ _ = error "TypeError"


