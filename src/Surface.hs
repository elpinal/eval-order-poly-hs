{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Surface
  ( Surface
  , TypeVar(..)
  , EvalVar(..)
  ) where

import Data.Functor.Classes

import EvalOrder
import Type

newtype TypeVar = TypeVar String
  deriving (Eq, Ord, Show)

newtype EvalVar = EvalVar String
  deriving (Eq, Ord, Show)

data Surface

type instance XForall Surface = TypeVar
type instance XD Surface = EvalVar
type instance XRec Surface = TypeVar
type instance XVar Surface = TypeVar
type instance XType Surface = EcoExt Surface

type instance XEVar Surface = EvalVar

deriving instance Show a => Show (TypeF Surface a)
deriving instance Show a => Show (EcoExt Surface a)

instance Show1 (TypeF Surface) where
  liftShowsPrec sp _ n x =
    showsPrec n $ fmap (\v -> IdString $ sp n v "") x

newtype IdString = IdString String

instance Show IdString where
  show (IdString x) = x
