--{-# LANGUAGE QuasiQuotes #-}

module Env where

import Data.Maybe
import Data.Typeable
import Data.Map 
import Prelude hiding(lookup)

--import Data.String.Interpolate

import Syntax
import Parser


-- data 

type Env = Map String TypeEnv  

data TypeEnv = Integer Integer 
             | TypeBool Bool
             deriving(Show)

getVal :: Env -> String -> Maybe TypeEnv  
getVal env ident  = lookup ident env 

-- ()
test :: Maybe TypeEnv -> TypeEnv  
test (Just x) = x
test Nothing = error "{} is Not yet defined "

setVal :: Env -> String -> TypeEnv -> Env 
setVal env ident val = insert ident val env
