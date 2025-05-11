module Eval.Patt.PatternPrimi where

import Control.Monad (msum)
import Data.DataType (LispVal (List))
import Data.Environment.EnvironmentType (MatchResult, Primi, ReplaceResult)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Eval.Patt.Regengine (
  ParsedPatt,
  ParsedRule,
  fromRule,
  internalReplace,
  patternMatching,
  runMatching,
  transformLispPattern,
 )


getMatchP :: ParsedPatt -> LispVal -> MatchResult
getMatchP p l = runMatching (patternMatching p l)


getMatch = getMatchP . transformLispPattern


-- | replace a lispval with a pattern matching specification
replaceP :: LispVal -> ParsedRule -> ReplaceResult
replaceP val (patt, target) = do
  matched <- getMatchP patt val
  return $ fmap (internalReplace target) matched


replace val (p, t) = replaceP val (transformLispPattern p, t)


-- | replace at the top level with a list of rule, return the first success, lazy state assures short circuit
replaceRuleListP :: LispVal -> [ParsedRule] -> ReplaceResult
replaceRuleListP val rules =
  fmap msum (mapM (replaceP val) rules)


replaceRuleList val = replaceRuleListP val . map fromRule


tryReplaceRuleListP :: LispVal -> [ParsedRule] -> Primi
tryReplaceRuleListP val = fmap (fromMaybe val) . replaceRuleListP val


tryReplaceRuleList val = tryReplaceRuleListP val . map fromRule


-- | replace all with a list of rule, top-down
replaceAllP :: [ParsedRule] -> LispVal -> Primi
replaceAllP rules val =
  let ifFailed =
        case val of
          List lis -> fmap List (mapM (replaceAllP rules) lis)
          _ -> return val
  in  do
        now <- replaceRuleListP val rules
        case now of
          Nothing -> ifFailed
          Just val -> return val


replaceAll rules = replaceAllP (map fromRule rules)
