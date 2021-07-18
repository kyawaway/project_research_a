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

type Env = IORef (Map String (IORef TypeEnv))



data TypeEnv = TypeInteger Integer 
             | TypeBool Bool
             | Closure Env String Statement
             | Null

instance Show TypeEnv where
        show (TypeInteger x) = show x
        show (TypeBool True) = show "true"
        show (TypeBool False) = show "false"
        show Closure {} = show "Closure"
        show Null = show "Null"
        

-- helper

nullEnv :: IO Env
nullEnv = newIORef Data.Map.empty 


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
                     return . lookup var >>=
                     return . maybe False (const True)


getVal :: Env -> String -> IO TypeEnv
getVal envRef var = readIORef envRef >>=
                    return . lookup var >>=
                    maybe (return Null) readIORef


setVal :: Env -> String -> TypeEnv -> IO TypeEnv
setVal envRef var val = do { 
    env <- readIORef envRef;
    maybe (return ()) (flip writeIORef val) (lookup var env);
    return val;
    }

defineVar :: Env -> String -> TypeEnv -> IO TypeEnv
defineVar envRef var val = do {
    alreadyDefined <- isBound envRef var;
    if alreadyDefined
        then setVal envRef var val
        else do {
            valRef <- newIORef val;
            env <- readIORef envRef;
            writeIORef envRef (insert var valRef env);
            return val;
        }
}

