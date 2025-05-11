module Eval.Primitive.IO.Print (printl) where

import Control.Monad.Trans (MonadTrans (lift))
import Data.DataType (atomNull)
import Data.Environment.EnvironmentType (Primi)
import qualified Data.Text.IO as T
import Eval.Patt.Pattern ()
import Eval.Primitive.PrimiFunc (getArgumentList)
import Show.Pretty (showLispVal)


printl :: Primi
printl = do
  vals <- getArgumentList
  let output = mconcat $ map showLispVal vals
   in (lift . lift) $ T.putStrLn output >> return atomNull
