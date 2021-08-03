module Env where

import Data.Maybe
import Data.Typeable
import Data.Map
import Data.IORef
import Prelude hiding(lookup)

--import Data.String.Interpolate

import Syntax
import Parser


-- data 

type Env = IORef [Map String (IORef TypeEnv)]



data TypeEnv = TypeInteger Integer
             | TypeBool Bool
             | Closure String Statement
             | Null

instance Show TypeEnv where
        show (TypeInteger x) = show x
        show (TypeBool True) = show "true"
        show (TypeBool False) = show "false"
        show (Closure arg body)  = "Closure, " ++  arg ++ ", " ++ show body  
        show Null = show "Null"


-- helper

nullEnv :: IO Env
nullEnv = newIORef [Data.Map.empty]

--- stack

push :: Env -> String -> TypeEnv -> IO Env 
push env var val = do
        valRef <- newIORef val
        cons <- readIORef env
        newIORef (Data.Map.fromList [(var, valRef)]:cons)


pop :: Env -> IO Env 
pop env = do
        garbage <- readIORef env
        case garbage of
            (h:t) -> do newIORef t
            [] -> error "stack error"

--- var

--- 基本方針は，スタックを頭から捜査

isBound :: Env -> String -> IO Bool
isBound envRef var = do
        envStack <- readIORef envRef
        case envStack of
            (h:t) -> case lookup var h of
                                 Just a -> return True
                                 Nothing -> do
                                     cdr <- newIORef t
                                     isBound cdr var
            [] -> return False



getVal :: Env -> String -> IO TypeEnv
getVal envRef var = do
        envStack <- readIORef envRef
        case envStack of
            (h:t) -> do
                cdr <- newIORef t
                maybe (getVal cdr var) readIORef (lookup var h)
            [] -> return Null


setVal :: Env -> String -> TypeEnv -> IO ()
setVal envRef var val = do
        envStack <- readIORef envRef
        case envStack of
            (h:t) -> do
                cdr <- newIORef t
                maybe (setVal cdr var val) (flip writeIORef val) (lookup var h)
            [] -> return ()


defineVar :: Env -> String -> TypeEnv -> IO ()
defineVar envRef var val = do
        alreadyDefined <- isBound envRef var
        if alreadyDefined
            then setVal envRef var  val
            else do
                valRef <- newIORef val
                envStack <- readIORef envRef
                case envStack of
                    (h:t) -> writeIORef envRef (insert var valRef h:t)
                    [] -> error "stack error"


