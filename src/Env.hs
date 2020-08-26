{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Env
  ( lookupVar
  , insert
  , insertFixpoint
  , Sort(..)
  , Environ

  , emptyEnv
  , EnvError
  , Env
  ) where

import Data.Coerce
import qualified Data.Map.Strict as Map

import Control.Effect.Throw
import Control.Effect.Reader

import Term
import Type (Type)

newtype Env x = Env (Map.Map Variable (Type x, Sort))

emptyEnv :: Env x
emptyEnv = Env mempty

data Sort
  = Ordinary
  | Fixpoint

data EnvError
  = Unbound Variable
  deriving (Eq, Show)

type Environ x sig m = (Has (Reader (Env x)) sig m, Has (Throw EnvError) sig m)

lookupVar :: Environ x sig m => Variable -> m (Type x, Sort)
lookupVar v = do
  Env m <- ask
  maybe (throwError $ Unbound v) return $ Map.lookup v m

insert :: forall x sig m a. Has (Reader (Env x)) sig m => Variable -> Type x -> m a -> m a
insert v ty = local @(Env x) $ coerce $ Map.insert v (ty, Ordinary)

insertFixpoint :: forall x sig m a. Has (Reader (Env x)) sig m => Variable -> Type x -> m a -> m a
insertFixpoint v ty = local @(Env x) $ coerce $ Map.insert v (ty, Fixpoint)
