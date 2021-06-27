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
getVal env var  = lookup var env 

-- ()
test :: Maybe Integer -> Integer 
test (Just x) = x
test Nothing = error "Not yet defined var"

setVal :: Env -> String -> Integer -> Env 
setVal env var val = insert var val env
