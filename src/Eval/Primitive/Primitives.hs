module Eval.Primitive.Primitives (primitives) where

import Data.Environment.EnvironmentType (Primi)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Eval.Primitive.Arithmatic.Arithmatic (
  dividel,
  logl,
  minusl,
  plusl,
  powerl,
  timesl,
 )
import Eval.Primitive.Compare.Compare (
  equall,
  greaterEquall,
  greaterl,
  inequalityl,
  lessEquall,
  lessl,
 )
import Eval.Primitive.Control.Branch (ifl)
import Eval.Primitive.Function.Lambda (functionl)
import Eval.Primitive.IO.Print (printl)
import Eval.Primitive.InOut.InOut (inl, outl)
import Eval.Primitive.List.List (
  applyl,
  carl,
  cdrl,
  consl,
  lengthl,
  mapl,
  partl,
  rangel,
 )
import Eval.Primitive.Logic.Logic (andl, notl, orl)
import Eval.Primitive.Nest.Nest (nestListl, nestl)
import Eval.Primitive.PrimiFunc (getArgumentList, many1op, noChange, withnop)
import Eval.Primitive.Replace.Replace (replaceAlll, replaceRepeatedl, replacel)
import Eval.Primitive.Set.Set (setDelayedl, setl, unsetl)


-- | Collections of all primitive function
primitives :: M.Map T.Text Primi
primitives =
  M.fromList
    [ ("CompoundExpression", compoundExpressionl)
    , ("Minus", minusl)
    , ("Divide", dividel)
    , ("Plus", plusl)
    , ("Times", timesl)
    , ("Power", powerl)
    , ("Log", logl)
    , -- list mainpulation
      ("car", carl)
    , ("cdr", cdrl)
    , ("cons", consl)
    , ("Length", lengthl)
    , ("Part", partl)
    , ("Map", mapl)
    , ("Apply", applyl)
    , -- list construction
      ("Range", rangel)
    , -- comparation
      ("Less", lessl)
    , ("LessEqual", lessEquall)
    , ("Greater", greaterl)
    , ("GreaterEqual", greaterEquall)
    , ("Equal", equall)
    , ("Inequality", inequalityl)
    , -- logic function
      ("Not", notl)
    , ("And", andl)
    , ("Or", orl)
    , -- branch
      ("If", ifl)
    , ("Function", functionl)
    , -- replace
      ("Replace", replacel)
    , ("ReplaceAll", replaceAlll)
    , ("ReplaceRepeated", replaceRepeatedl)
    , ("Nest", nestl)
    , ("NestList", nestListl)
    , ("Set", setl)
    , ("SetDelayed", setDelayedl)
    , ("Print", printl)
    , ("In", inl)
    , ("Out", outl)
    , ("Condition", conditionl)
    , ("Pattern", patternl)
    , -- , TODO: ("GetAttributes", getAttributesl)
      -- , TODO: ("SetAttributes", setAttributesl)
      -- , TODO: ("Module", modulel)
      ("Unset", unsetl)
    ]


compoundExpressionl :: Primi
compoundExpressionl = do
  many1op
  fmap last getArgumentList


conditionl :: Primi
conditionl = do
  withnop 2
  noChange


patternl :: Primi
patternl = do
  withnop 2
  noChange
