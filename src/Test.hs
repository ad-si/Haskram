module Test where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.State (evalStateT)
import Data.DataType (
  LispVal (Atom, List, Number),
  extractValue,
  integer,
 )
import Data.Environment.Environment ()
import Data.Number.Number (Number (Rational))
import Eval.Eval (Primi, eval, eval', initialState)
import Parser.Trans (readExpr)
import System.IO.Unsafe (unsafePerformIO)


addHead a b = List (Atom a : b)


list = addHead "List"


plus = addHead "Plus"


times = addHead "Times"


comp = addHead "CompoundExpression"


part = addHead "Part"


map' = addHead "Map"
mapAll = addHead "MapAll"
apply = addHead "Apply"
apply1 [l1, l2] = apply [l1, l2, list [one]]


replace = addHead "ReplaceAll"
replaceR = addHead "ReplaceRepeated"
rule = addHead "Rule"
ruleD = addHead "RuleDelayed"


set = addHead "Set"
setD = addHead "SetDelayed"


unset = addHead "Unset" . return


fun = addHead "Function"
slot = addHead "Slot" . return
s1 = slot one
s2 = slot two
ss1 = addHead "SlotSequence" [one]


cond = addHead "Condition"


deriv n l = List [List [Atom "Derivative", integer n], l]


fact = addHead "Factorial" . return
fact2 = addHead "Factorial2" . return


patt = addHead "Pattern"
pattT = addHead "PatternTest"
blk = List [Atom "Blank"]


andE = addHead "And"
orE = addHead "Or"
notE = addHead "Not"
ineq = addHead "Inequality"


dot = addHead "Dot"


alter = addHead "Alternatives"


equal = Atom "Equal"
less = Atom "Less"
lessEq = Atom "LessEqual"
great = Atom "Greater"
greatEq = Atom "GreaterEqual"
unEq = Atom "Unequal"


one = integer 1
two = integer 2
three = integer 3


pe = Atom "P"


rational = Number . Rational


readVal = extractValue . readExpr


testEvalWith :: (LispVal -> Primi) -> String -> LispVal
testEvalWith eval expr =
  let val = readVal expr
      evaled = unsafePerformIO . runExceptT $ evalStateT (eval val) initialState
  in  extractValue evaled


testEvalOnce = testEvalWith eval'
runEval = testEvalWith eval
