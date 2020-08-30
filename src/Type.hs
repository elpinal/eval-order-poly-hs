{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Type
  ( Type
  , TypeF(..)

  , XForall
  , XD
  , XRec
  , XVar
  , XType

  , EcoExt(..)

  , getUnit
  , getProd
  , getSum
  , getArrow
  , getForall

  , TypeStructureError(..)
  ) where

import Control.Effect.Throw
import Data.Functor.Foldable
import Data.Text.Prettyprint.Doc

import EvalOrder

-- Note: don't forget to update `Economical.equiv` function as this definition evolves.
data TypeF x a
  = Unit
  | Var (XVar x)
  | Forall (XForall x) a
  | Arrow a a
  | Prod a a
  | Sum a a
  | Rec (XRec x) a
  | XType !(XType x a)

deriving instance Functor (XType x) => Functor (TypeF x)

type Type x = Fix (TypeF x)

data EcoExt x a
  = D (XD x) a
  | Suspend (EvalOrder x) a
  deriving Functor

-- TODO: Combine `XForall` and `XRec`.
type family XForall x
type family XD x
type family XRec x
type family XVar x

type family XType x :: * -> *

data TypeStructureError x
  = NotUnit (Type x)
  | NotProd (Type x)
  | NotSum (Type x)
  | NotArrow (Type x)
  | NotForall (Type x)

instance Pretty (Type x) => Pretty (TypeStructureError x) where
  pretty = \case
    NotUnit ty   -> "not unit type:" <+> pretty ty
    NotProd ty   -> "not product type:" <+> pretty ty
    NotSum ty    -> "not sum type:" <+> pretty ty
    NotArrow ty  -> "not arrow type:" <+> pretty ty
    NotForall ty -> "not universal type:" <+> pretty ty

getUnit :: Has (Throw (TypeStructureError x)) sig m => Type x -> m ()
getUnit (Fix Unit) = return ()
getUnit ty         = throwError $ NotUnit ty

getProd :: Has (Throw (TypeStructureError x)) sig m => Type x -> m (Type x, Type x)
getProd (Fix (Prod x y)) = return (x, y)
getProd ty               = throwError $ NotProd ty

getSum :: Has (Throw (TypeStructureError x)) sig m => Type x -> m (Type x, Type x)
getSum (Fix (Sum x y)) = return (x, y)
getSum ty              = throwError $ NotSum ty

getArrow :: Has (Throw (TypeStructureError x)) sig m => Type x -> m (Type x, Type x)
getArrow (Fix (Arrow x y)) = return (x, y)
getArrow ty                = throwError $ NotArrow ty

getForall :: Has (Throw (TypeStructureError x)) sig m => Type x -> m (XForall x, Type x)
getForall (Fix (Forall x y)) = return (x, y)
getForall ty                 = throwError $ NotForall ty
