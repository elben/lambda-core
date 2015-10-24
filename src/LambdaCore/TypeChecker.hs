module LambdaCore.TypeChecker where

import LambdaCore.Data
import LambdaCore.Utils

import Control.Monad.Trans.Except
import Control.Monad.Trans.State

type TypeCheck a = ExceptT TypeError (State FreshCounter) a

checkType :: Exp -> TypeCheck Type
checkType _ = return TUnit

