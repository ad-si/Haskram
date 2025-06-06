module Eval.Primitive.Replace.Replace (
  -- | Replace Functions
  replacel,
  replaceAlll,
  replaceRepeatedl,
) where

import Control.Monad.Except ()
import Control.Monad.Trans.Class (lift)
import Data.DataType (LispVal)
import Data.Environment.Environment ()
import Data.Environment.EnvironmentType (EvalArguments, Primi)
import Data.Environment.Update ()
import Eval.Patt.PatternPrimi (replaceAllP, tryReplaceRuleListP)
import Eval.Primitive.List.Level (unpackLevelSpeci)
import Eval.Primitive.PrimiFunc (between, evaluate, usesArgumentError, withnop)
import Eval.Primitive.Replace.Unpack (unpackReplaceArg)


replacel, replaceAlll :: Primi
replacel = do
  between 2 3
  usesArgumentError replacel'
replaceAlll = do
  withnop 2
  usesArgumentError replaceAlll'


replacel' :: EvalArguments
replacel' (expr : rules : level) = do
  unpackedRules <- lift $ unpackReplaceArg rules
  levelSpeci <- lift $ unpackLevelSpeci 0 level
  levelSpeci (`tryReplaceRuleListP` unpackedRules) expr


replaceAlll' :: EvalArguments
replaceAlll' [expr, rules] = do
  unpackedRules <- lift $ unpackReplaceArg rules
  replaceAllP unpackedRules expr


-- functions relating with replace repeated feature
replaceRepeatedl :: Primi
replaceRepeatedl = do
  withnop 2
  usesArgumentError replaceRepeatedl'


-- | Replace until yielding no new result
replaceRepeated :: LispVal -> (LispVal -> Primi) -> Primi
replaceRepeated old replace = do
  new <- replace old >>= evaluate
  if new == old
    then
      return new
    else
      replaceRepeated new replace


replaceRepeatedl' :: [LispVal] -> Primi
replaceRepeatedl' [expr, rules] = do
  unpackedRules <- lift $ unpackReplaceArg rules
  replaceRepeated expr (replaceAllP unpackedRules)
