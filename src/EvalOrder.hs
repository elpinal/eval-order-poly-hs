{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EvalOrder
  ( EvalOrder(..)
  , mapVar
  , Valueness(..)

  , XEVar
  ) where

data EvalOrder x
  = V -- call by value
  | N -- call by name
  | EVar (XEVar x)

deriving instance Show (XEVar x) => Show (EvalOrder x)

type family XEVar x

mapVar :: Applicative f => (XEVar a -> f (XEVar b)) -> EvalOrder a -> f (EvalOrder b)
mapVar _ V        = pure V
mapVar _ N        = pure N
mapVar f (EVar v) = EVar <$> f v

data Valueness
  = Val
  | Top
  deriving (Eq, Show)

-- "Join" operation.
instance Semigroup Valueness where
  Val <> Val = Val
  _ <> _     = Top

instance Monoid Valueness where
  mempty = Val
