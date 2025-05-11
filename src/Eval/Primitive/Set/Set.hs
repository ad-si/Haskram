module Eval.Primitive.Set.Set (setl, setDelayedl, unsetl) where

import Control.Monad ()
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class ()
import Control.Monad.Trans.Except ()
import Data.DataType (LispError (SetError), LispVal, atomNull)
import Data.Environment.Environment (unset, validSet)
import Data.Environment.EnvironmentType (Pattern, Primi, StateResult)
import Data.Environment.Update (setVariable)
import Data.IORef ()
import Eval.Patt.Pattern ()
import Eval.Primitive.PrimiFunc (getArgumentList, updateCon, withnop)


setl :: Primi
setl = do
  [lhs, rhs] <- getArgumentList
  setVar lhs rhs
  return rhs


setDelayedl :: Primi
setDelayedl = do
  setl
  return atomNull


setVar :: Pattern -> LispVal -> StateResult ()
setVar lhs rhs =
  if validSet lhs
    then setVariable lhs rhs
    else
      throwError $ SetError lhs


unsetl :: Primi
unsetl = do
  withnop 1
  [a] <- getArgumentList
  updateCon (unset a)
  return a
