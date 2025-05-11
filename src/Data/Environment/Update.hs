module Data.Environment.Update where

import Data.DataType (LispVal)
import Data.Environment.Environment (
  replaceContext,
  updateContext,
 )
import Data.Environment.EnvironmentType (Primi, StateResult, con)
import Eval.Primitive.PrimiFunc (updateCon)

import Control.Lens (use)


setVariable :: LispVal -> LispVal -> StateResult ()
setVariable lhs rhs = updateCon (updateContext lhs rhs)


getVariable :: LispVal -> Primi
getVariable lhs = do
  context <- use con
  replaceContext lhs context
