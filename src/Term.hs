{-# LANGUAGE DeriveFunctor #-}

module Term
  ( Term
  , TermF(..)
  , Variable(..)
  , Index(..)
  , index

  , getForm
  ) where

import Data.Functor.Foldable

import qualified Type as T

newtype Variable = Variable String
  deriving (Eq, Ord, Show)

data Index
  = Fst
  | Snd
  deriving (Eq, Show)

index :: Index -> a -> a -> a
index Fst x _ = x
index Snd _ y = y

data TermF x a
  = Unit
  | Var Variable
  | Abs Variable a
  | App a a
  | Fix Variable a
  | Gen (T.XForall x) a
  | Inst a (T.Type x)
  | Ascribe a (T.Type x)
  | Pair a a
  | Proj Index a
  | Inj Index a
  | Case a Variable a Variable a
  deriving Functor

type Term x = Fix (TermF x)

getForm :: TermF x a -> String
getForm t = case t of
  Unit       -> "()"
  Var _      -> "variable"
  Abs{}      -> "lambda abstraction"
  App{}      -> "application"
  Term.Fix{} -> "fix"
  Gen{}      -> "generalization"
  Inst{}     -> "instantiation"
  Ascribe{}  -> "ascription"
  Pair{}     -> "pair"
  Proj{}     -> "projection"
  Inj{}      -> "injection"
  Case{}     -> "match-expresson"
