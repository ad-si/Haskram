-- {-# LANGUAGE ExistentialQuantification #-}
module Eval.Eval (
  eval,
  evalWithRecord,
  eval',
  initialState,
  Primi,
  StateResult,
) where

import Control.Lens (use, uses, (-=), (.=))
import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Trans.State ()
import Data.Attribute (
  Attribute (HoldAll, HoldFirst, HoldRest),
  Attributes,
  allAttr,
  attributeTransform,
  getAttributes,
  initAttributes,
 )
import Data.DataType (
  LispError (LimitExceed),
  LispVal (Atom, List, Number),
  atomIn,
  atomLimit,
  atomOut,
  integer,
 )
import Data.Environment.Environment (
  nullContext,
  replaceContext,
  validSet,
 )
import Data.Environment.EnvironmentType (
  Primi,
  PrimiEnv (PrimiEnv),
  StateResult,
  args,
  attr,
  con,
  dep,
  line,
 )
import Data.Environment.Update (getVariable, setVariable)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Number.Number (Number (Rational))
import Data.Ratio (denominator, numerator)
import qualified Data.Text as T
import Eval.Primitive.Function.Lambda (evalLambda)
import Eval.Primitive.PrimiFunc (evaluate, noChange, stateThrow)
import Eval.Primitive.Primitives (primitives)


initialState = PrimiEnv eval nullContext [] initAttributes 4096 1


evalWithRecord :: LispVal -> Primi
evalWithRecord val = do
  (Number limit) <- getVariable atomLimit
  dep .= fromIntegral limit
  updateInOut atomIn val
  evaled <- eval val
  updateInOut atomOut evaled
  return evaled


updateInOut :: LispVal -> LispVal -> StateResult ()
updateInOut atom val = do
  n <- uses line integer
  setVariable (List [atom, n]) val


checkLimit :: StateResult ()
checkLimit = do
  exceed <- uses dep (<= 0)
  if exceed
    then
      stateThrow LimitExceed
    else
      dep -= 1


eval :: LispVal -> Primi
eval val = do
  checkLimit
  x1 <- eval' val
  if x1 == val then return x1 else eval x1


eval' :: LispVal -> Primi
eval' (List (v : vs)) = do
  headE <- eval v
  attributes <- use attr
  arguments <- attributeEvaluateArgs headE vs
  args .= headE : arguments
  attTransform attributes <$> evalHead headE
eval' (Atom "$Line") = uses line integer
eval' val@(Atom _) = use con >>= replaceContext val
eval' n@(Number (Rational r))
  | denominator r == 1 = return (integer $ numerator r)
  | otherwise = return n
eval' x = return x


-- eval head --------------------------------------
evalPrimitiveHead :: LispVal -> Maybe Primi
evalPrimitiveHead (Atom name) =
  M.lookup name primitives


evalWithEnv :: Primi
evalWithEnv = do
  lhs <- noChange
  if validSet lhs
    then use con >>= replaceContext lhs
    else noChange


evalHead :: LispVal -> Primi
evalHead h@(Atom _) =
  fromMaybe evalWithEnv (evalPrimitiveHead h)
evalHead (List (Atom "Function" : rest)) =
  evalLambda
evalHead _ = noChange


-- ------------------------------------------------

-- attribute relating functions

-- | evaluate arguments under the attributes specification of Head
attributeEvaluateArgs
  :: LispVal -> [LispVal] -> StateResult [LispVal]
attributeEvaluateArgs h rests = do
  attributes <- use attr
  let att = getAttributes h attributes
  evaled <- attEvalHold att rests
  return $ allAttr att h evaled


-- | handle HoldAll HoldFirst HoldRest
attEvalHold
  :: [Attribute] -> [LispVal] -> StateResult [LispVal]
attEvalHold atts vals
  | elem HoldAll atts = return vals
  | elem HoldFirst atts = do
      rest <- mapM evaluate (tail vals)
      return (head vals : rest)
  | elem HoldRest atts = do
      first <- evaluate (head vals)
      return (first : tail vals)
  | otherwise = mapM evaluate vals


attTransform :: Attributes -> LispVal -> LispVal
attTransform attributes val = attributeTransform attributes val
