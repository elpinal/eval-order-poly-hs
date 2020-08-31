{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Target
  ( Term
  , TermF(..)
  ) where

import Data.Functor.Foldable

import Term (Variable, Index)
import Type

data Target

data V a

type instance XType Target = V

data TermF a
  = Unit
  | Var Variable
  | Abs Variable a
  | App a a
  | Fix Variable a
  | Gen a
  | Inst a
  | Pair a a
  | Proj Index a
  | Inj Index a
  | Case a Variable a Variable a
  | Thunk a
  | Force a
  | Roll a
  | Unroll a
  deriving Functor

type Term = Fix TermF
