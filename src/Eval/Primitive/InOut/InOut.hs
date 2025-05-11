module Eval.Primitive.InOut.InOut (inl, outl) where

import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except ()
import Data.DataType (
  IOThrowsError,
  LispError (Default),
  LispVal (Atom, List),
  atomLine,
  unpackInt,
 )
import Data.Environment.Environment (replaceContext)
import Data.Environment.EnvironmentType (Context, Primi)
import Data.Environment.Update ()
import Data.IORef ()
import qualified Data.Text as T
import Eval.Primitive.PrimiFunc (getArgs, getCon, withnop)


inl, outl :: Primi
inl = indexl
outl = indexl


unpackError name =
  Default ("Machine-sized integer is expected in " `T.append` name)


-- unpack the index arguement in In or Out
unpack :: T.Text -> LispVal -> IOThrowsError Integer
unpack name =
  unpackInt (unpackError name)


plusLine :: LispVal -> LispVal
plusLine val = List [Atom "Plus", atomLine, val]


index :: LispVal -> Context -> [LispVal] -> Primi
index fun@(Atom name) context [n] = do
  n' <- lift (unpack name n)
  if n' >= 0
    then
      replaceContext (List [fun, n]) context
    else
      return $ List [fun, plusLine n]


indexl :: Primi
indexl = do
  withnop 1
  context <- getCon
  h : args <- getArgs
  index h context args
