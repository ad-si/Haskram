{-# LANGUAGE FlexibleContexts #-}

module Eval.Primitive.Nest.Nest (nestl, nestListl) where

import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Trans.Class (lift)
import Data.DataType (
  LispError (Default),
  LispVal,
  applyHead,
  list,
  unpackIntWithThre,
 )
import Data.Environment.EnvironmentType (Primi)
import Data.Number.Number ()
import Eval.Primitive.PrimiFunc (evaluate, getArgumentList, withnop)
import Eval.Primitive.Replace.Replace ()


nestl, nestListl :: Primi
nestl = nestUnpack nest
nestListl = nestUnpack nestList


type Nest = LispVal -> LispVal -> Int -> Primi


nest, nestList :: Nest
nest _ arg 0 = return arg
nest f arg n = do
  evaled <- evaluate (applyHead f arg)
  nest f evaled (n - 1)


nestList' _ arg 0 = return [arg]
nestList' f arg n = do
  evaled <- evaluate (applyHead f arg)
  rest <- nestList' f evaled (n - 1)
  return $ arg : rest
nestList f arg n = fmap list (nestList' f arg n)


nestErr = Default "Nest :: non-negative machine-sized number expected"


nestUnpack :: Nest -> Primi
nestUnpack nest = do
  withnop 3
  [f, arg, n] <- getArgumentList
  n' <- lift $ unpackIntWithThre 0 nestErr n
  nest f arg n'
