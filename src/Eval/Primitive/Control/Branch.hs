module Eval.Primitive.Control.Branch (ifl) where

import Data.DataType (LispVal, isBool, trueQ)
import Data.Environment.EnvironmentType (Primi)
import Data.Maybe (fromMaybe)
import Eval.Primitive.PrimiFunc (between, usesArgumentMaybe)


ifl :: Primi
ifl = do
  between 3 4
  usesArgumentMaybe ifl'


if3Args :: [LispVal] -> Maybe LispVal
if3Args [predict, r1, r2]
  | isBool predict = Just $ if trueQ predict then r1 else r2
  | otherwise = Nothing


ifl' :: [LispVal] -> Maybe LispVal
ifl' args
  | length args == 3 = if3Args args
  | otherwise = Just $ fromMaybe (last args) (if3Args (init args))
