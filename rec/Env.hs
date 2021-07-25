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

type Env = IORef EnvStack

data EnvStack = Gloval (Map String (IORef TypeEnv))
              | Local (Map String (IORef TypeEnv)) Env


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
nullEnv = newIORef (Gloval Data.Map.empty)


isBound :: Env -> String -> IO Bool
isBound envRef var = do
        envStack <- readIORef envRef
        case envStack of
            Gloval e -> case lookup var e of
                            Just a -> return True
                            Nothing -> return False
            Local car cdr -> case lookup var car of
                                 Just a -> return True
                                 Nothing -> isBound cdr var

{-
isBound envRef var = readIORef envRef >>=
                     return . lookup var >>=
                     return . maybe False (const True)
-}



getVal :: Env -> String -> IO TypeEnv
getVal envRef var = do
        envStack <- readIORef envRef
        case envStack of
            Gloval e -> maybe (return Null) readIORef (lookup var e)
            Local car cdr -> maybe (getVal cdr var) readIORef (lookup var car)

{-
getVal envRef var = readIORef envRef >>=
                    return . lookup var >>=
                    maybe (return Null) readIORef
-}



setVal :: Env -> String -> TypeEnv -> IO TypeEnv
setVal envRef var val = do
        envStack <- readIORef envRef
        case envStack of
            Gloval e -> do
                hoge <- maybe (return ()) (flip writeIORef val) (lookup var e)
                return val
            Local car cdr -> do
                huga <- maybe (return ()) (flip writeIORef val) (lookup var car)
                return val


{-
setVal envRef var val = do { 
    env <- readIORef envRef;
    maybe (return ()) (flip writeIORef val) (lookup var env);
    return val;
    }
-}



defineVar :: Env -> String -> TypeEnv -> IO TypeEnv
defineVar envRef var val = do
        alreadyDefined <- isBound envRef var
        if alreadyDefined
            then setVal envRef var  val
            else do
                valRef <- newIORef val
                envStack <- readIORef envRef
                case envStack of
                    Gloval e -> do
                        hoge <- writeIORef envRef (Gloval (insert var valRef e))
                        return val
                    Local car cdr -> do
                        huga <- writeIORef envRef (Local (insert var valRef car) cdr)
                        return val



{-
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
-}

push :: Env -> String -> TypeEnv -> IO Env 
push env var val = do
        valRef <- newIORef val
        newIORef (Local (Data.Map.fromList [(var, valRef)]) env)
{-
pop :: Env -> Env 
pop env = do
        garbage <- readIORef env
        case garbage of
            Local car cdr -> cdr
            Gloval e -> error "error"
-}


