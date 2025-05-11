{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((+=), (^.))
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (runStateT))
import Data.DataType (IOThrowsError, LispVal (Atom), liftThrows)
import Data.Environment.Environment ()
import Data.Environment.EnvironmentType (PrimiEnv, StateResult, line)
import Data.Foldable (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Eval.Eval (evalWithRecord, initialState)
import Eval.Primitive.PrimiFunc (getLineNumber)
import Parser.Trans (readExpr)
import Show.Pretty (showLispVal)
import System.Console.Haskeline (
  InputT,
  defaultSettings,
  getInputLine,
  outputStrLn,
  runInputT,
 )
import System.IO ()
import Text.Printf (printf)


info :: T.Text
info =
  T.unlines
    [ "Haskram - A Wolfram Language interpreter written in Haskell"
    , "Version 0.1.0"
    ]


loop :: PrimiEnv -> InputT IO ()
loop env = do
  let cl = env ^. line
  input <- getInputLine (printf "In[%d]:= " cl :: String)
  forM_ input (repl env)


repl :: PrimiEnv -> String -> InputT IO ()
repl env input = do
  res <- lift (runExceptT $ runStateT (evaluateExpression input) env)
  case res of
    Right (ans, newEnv) -> do
      let ncl = newEnv ^. line
      when (ans /= "") $ outputStrLn ans
      loop newEnv
    Left err -> do
      outputStrLn (show err)
      loop env


type Repl = InputT (StateT PrimiEnv IOThrowsError) ()


getExpr :: String -> IOThrowsError LispVal
getExpr string =
  liftThrows (readExpr string)


evaluateExpression :: String -> StateResult String
evaluateExpression str = do
  expr <- lift (getExpr str)
  res <- evalWithRecord expr
  new <- getLineNumber
  line += 1
  return (report new res)


report :: Int -> LispVal -> String
report _ (Atom "Null") = ""
report n val = printf "Out[%d]= " n ++ T.unpack (showLispVal val)


main :: IO ()
main = do
  T.putStrLn info
  runInputT defaultSettings (loop initialState)
