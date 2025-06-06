module Eval.Primitive.Replace.Unpack (unpackReplaceArg) where

import Control.Monad.Except (MonadError (throwError))
import Data.DataType (
  IOThrowsError,
  LispError (Default),
  LispVal (Atom, List),
  tshow,
 )
import Data.Environment.EnvironmentType ()
import qualified Data.Text as T
import Eval.Patt.Regengine (ParsedRule, transformLispPattern)


reps val = Default (tshow val `T.append` " cannot be used for replacing.")


unpack :: LispVal -> Maybe ParsedRule
unpack (List [Atom "Rule", a, b]) = Just (transformLispPattern a, b)
unpack (List [Atom "RuleDelayed", a, b]) = Just (transformLispPattern a, b)
unpack _ = Nothing


-- | unpack rule(s) arguemnts in function like Replace, ReplaceAll, etc.
unpackReplaceArg :: LispVal -> IOThrowsError [ParsedRule]
unpackReplaceArg val =
  let err = throwError (reps val)
      fromUnpackMaybe = maybe err return
  in  case val of
        List (Atom "List" : rules) ->
          fromUnpackMaybe (mapM unpack rules)
        rule -> fromUnpackMaybe (fmap return (unpack rule))
