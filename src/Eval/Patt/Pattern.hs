module Eval.Patt.Pattern where

import Control.Monad ()
import Data.DataType (LispVal (Atom, List))
import Data.Environment.EnvironmentType (Pattern)
import Data.Function (on)
import Data.Maybe ()
import qualified Data.Text as T


blankQ :: Pattern -> Bool
blankQ (List (Atom "Blank" : _)) = True
blankQ _ = False


blankEq :: Pattern -> Pattern -> Bool
blankEq a b
  | blankQ a && blankQ b = True
  | otherwise = False


blankEqui = blankEq `on` unpackPatt


unpackPatt :: Pattern -> Pattern
unpackPatt (List [Atom "Pattern", _, patt]) = unpackPatt patt
unpackPatt other = other


patternEqui :: Pattern -> Pattern -> Bool
patternEqui (List as) (List bs) =
  let el = length as == length bs
      pl = and $ zipWith patternEqui as bs
  in  el && pl
patternEqui a b = a == b || blankEqui a b


isPattern :: LispVal -> Bool
isPattern (Atom "Pattern") = True
isPattern (Atom "Blank") = True
isPattern (Atom "BlankSequence") = True
isPattern (Atom "BlankNullSequence") = True
isPattern (List xs) = any isPattern xs
isPattern _ = False
