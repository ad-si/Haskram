module Eval.Primitive.List.Map (mapl, applyl) where

import Data.DataType (
  IOThrowsError,
  LispVal,
  applyHead,
  changeHead,
 )
import Data.Environment.EnvironmentType (Primi)
import Data.Number.Number ()
import Eval.Primitive.List.Level (unpackNormalLevelSpeci)
import Eval.Primitive.PrimiFunc (between, getArgumentList)

import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Trans.Class (lift)


mapl :: Primi
mapl = do
  between 2 3
  args <- getArgumentList
  lift (unpackArgs applyHead 1 args)


applyl :: Primi
applyl = do
  between 2 3
  args <- getArgumentList
  lift (unpackArgs changeHead 0 args)


mapl' = unpackArgs applyHead 1
applyl' = unpackArgs changeHead 0


unpackArgs
  :: (LispVal -> LispVal -> LispVal)
  -> Int
  -> [LispVal]
  -> IOThrowsError LispVal
unpackArgs fun def (f : app : speci) = do
  speciMap <- unpackNormalLevelSpeci def speci
  return (speciMap (fun f) app)
