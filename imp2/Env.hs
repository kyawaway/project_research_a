module Env where

import Data.Maybe
import Data.Typeable
import Data.Map 
import Prelude hiding(lookup)

import Syntax
import Parser


-- data 

type Env = Map String TypeEnv  

data TypeEnv = TypeInteger Integer 
             | TypeBool Bool

getVal :: Env -> String -> Maybe TypeEnv  
getVal env ident  = lookup ident env 

-- ()
test :: Maybe TypeEnv -> TypeEnv  
test (Just x) = x


setVal :: Env -> String -> TypeEnv -> Env 
setVal env ident val = insert ident val env
