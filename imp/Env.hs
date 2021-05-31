module Env where

import Data.Maybe
import Data.Typeable
import Data.Map 
import Prelude hiding(lookup)

import Syntax
import Parser


-- data 

type Env = Map String Integer 

getVal :: Env -> String -> Maybe Integer 
getVal env id  = lookup id env 

-- ()
test :: Maybe Integer -> Integer 
test (Just x) = x


setVal :: Env -> String -> Integer -> Env 
setVal env id val = insert id val env
