module Eval.Primitive.List.Cons (
  -- * List Construction function
  rangel,
) where

import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Trans.Class (lift)
import Data.DataType (
  IOThrowsError,
  LispVal (Number),
  list,
  unpackNum',
 )
import Data.Environment.EnvironmentType (Primi)
import Data.Number.Number (Number, isDouble, toNumberDouble)
import Eval.Primitive.PrimiFunc (between, getArgumentList)


rangel :: Primi
rangel = do
  between 1 3
  ls <- getArgumentList
  lift (rangel' ls)


rangel' :: [LispVal] -> IOThrowsError LispVal
rangel' ls = do
  ns <- toRangeArgs ls
  return $ fromNumberList (rangeLP ns)


rangeLP :: [Number] -> [Number]
rangeLP [n] = range 1 n 1
rangeLP [f, t] = range f t 1
rangeLP [f, t, d] = range f t d


fromNumberList :: [Number] -> LispVal
fromNumberList = list . map Number


range :: Number -> Number -> Number -> [Number]
range i j d =
  let n = truncate $ (j - i) / d
  in  rangeFrom n i d


rangeFrom :: Int -> Number -> Number -> [Number]
rangeFrom n i d = take (n + 1) (iterate (+ d) i)


toRangeArgs :: [LispVal] -> IOThrowsError [Number]
toRangeArgs ls = do
  ns <- mapM unpackNum' ls
  return $ toListDouble ns


toListDouble :: [Number] -> [Number]
toListDouble ls
  | any isDouble ls = map toNumberDouble ls
  | otherwise = ls
