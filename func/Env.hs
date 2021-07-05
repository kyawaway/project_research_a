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
             | Null
             deriving (Show)




nullEnv :: IO Env
nullEnv = newIORef Data.Map.empty 


{-
getVal :: Env -> String -> IO TypeEnv  
getVal env ident  = lookup ident env 

-- ()
test :: Maybe TypeEnv -> TypeEnv  
test (Just x) = x
test Nothing = error "{} is Not yet defined "


setVal :: Env -> String -> TypeEnv -> Env     
setVal env ident val = insert ident val env
-}


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
                     return . lookup var >>=
                     return . maybe False (const True)


getVal :: Env -> String -> IO TypeEnv
getVal envRef var = readIORef envRef >>=
                    return . lookup var >>=
                    maybe (return Null) readIORef

{-
test :: Maybe (IO TypeEnv) -> IO TypeEnv
test (Just x) = x
test Nothing = error "{} is Not yet defined"


getVal :: Env -> String -> IO TypeEnv  
getVal envRef ident = do 
        env <- readIORef envRef
        test (lookup ident env)

-}
--getVal env ident  = test (lookup ident env)

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
            writeIORef envRef (fromList [(var, valRef)]);
            return val;
        }
}

