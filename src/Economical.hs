{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Economical
  ( Economical
  , FEVar
  , freshVar
  , fromSurface

  , check
  , synthesize
  , synth

  , TypeError
  , Errors

  , equiv
  , Variable(..)
  ) where

import Data.Bifunctor
import Data.Coerce
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Classes
import Data.Functor.Foldable
import qualified Data.Map.Strict as Map

import Control.Carrier.Throw.Either
import qualified Control.Carrier.Fresh.Strict as F
import qualified Control.Carrier.Reader as R
import Control.Effect.Labelled
import Control.Effect.Reader.Labelled

import qualified Env
import EvalOrder as EO
import Surface
import qualified Term as T
import Type

data Economical

type instance XForall Economical = ()
type instance XD Economical = ()
type instance XRec Economical = ()
type instance XVar Economical = Variable FTVar
type instance XType Economical = EcoExt Economical

type instance XEVar Economical = Variable FEVar

data Variable a
  = Bound Int
  | Free a
  deriving Show

newtype FTVar = FTVar Int
  deriving (Eq, Show)

newtype FEVar = FEVar Int
  deriving (Eq, Show)

class Fresh a where
  freshVar :: Has F.Fresh sig m => m a

instance Fresh FTVar where
  freshVar = coerce <$> F.fresh

instance Fresh FEVar where
  freshVar = coerce <$> F.fresh

deriving instance Show a => Show (TypeF Economical a)
deriving instance Show a => Show (EcoExt Economical a)

instance Show1 (TypeF Economical) where
  liftShowsPrec sp _ n x =
    showsPrec n $ fmap (\v -> IdString $ sp n v "") x

newtype IdString = IdString String

instance Show IdString where
  show (IdString x) = x

data ScopeError
  = UnboundTVar
  | UnboundEVar
  deriving (Eq, Show)

fromSurface :: (Functor (f Surface), FromSurface f)
               => Fix (f Surface) -> Either ScopeError (Fix (f Economical))
fromSurface ty = run $ runThrow $
  R.runReader (0, Map.empty) $ runLabelled @"ev" $
  R.runReader (0, Map.empty) $ runLabelled @"tv" $ cata fromSurface' ty

type Alg f x a = f x a -> a

class FromSurface f where
  fromSurface' ::
    ( Has (Throw ScopeError) sig m
    , HasLabelled "ev" (Reader (Int, Map.Map EvalVar Int)) sig m
    , HasLabelled "tv" (Reader (Int, Map.Map TypeVar Int)) sig m
    )
    => Alg f Surface (m (Fix (f Economical)))

instance FromSurface TypeF where
  fromSurface' = \case
    Unit   -> return $ Fix Unit
    Var tv -> do
      (d, mm) <- asks @"tv" $ second $ Map.lookup tv
      maybe (throwError UnboundTVar) (return . Fix . Var . Bound . (\x -> d - x - 1)) mm
    Forall tv x    -> Fix . Forall () <$> local @"tv" (\(d, m) -> (d + 1, Map.insert tv d m)) x
    Arrow x y      -> (Fix .) . Arrow <$> x <*> y
    Prod x y       -> (Fix .) . Prod <$> x <*> y
    Sum x y        -> (Fix .) . Sum <$> x <*> y
    Rec tv x       -> Fix . Rec () <$> local @"tv" (\(d, m) -> (d + 1, Map.insert tv d m)) x
    XType (D ev x) -> Fix . XType . D () <$> local @"ev" (\(d, m) -> (d + 1, Map.insert ev d m)) x
    XType (Suspend eo x) -> do
      eo' <- (`EO.mapVar` eo) $ \ev -> do
         (d, mm) <- asks @"ev" $ second $ Map.lookup ev
         maybe (throwError UnboundEVar) (return . Bound . (\x -> d - x - 1)) mm
      Fix . XType . Suspend eo' <$> x

instance FromSurface T.TermF where
  fromSurface' = \case
    T.Unit -> return $ Fix T.Unit
    T.Var v -> return $ Fix $ T.Var v
    T.Abs v x -> Fix . T.Abs v <$> x
    T.App x y -> (Fix .) . T.App <$> x <*> y
    T.Fix v x -> Fix . T.Fix v <$> x
    T.Gen tv x -> Fix . T.Gen () <$> local @"tv" (\(d, m) -> (d + 1, Map.insert tv d m)) x
    T.Inst x ty -> (Fix .) . T.Inst <$> x <*> cata fromSurface' ty
    T.Ascribe x ty -> (Fix .) . T.Ascribe <$> x <*> cata fromSurface' ty
    T.Pair x y -> (Fix .) . T.Pair <$> x <*> y
    T.Proj i x -> Fix . T.Proj i <$> x
    T.Inj i x -> Fix . T.Inj i <$> x
    T.Case x v1 y v2 z -> do
      x' <- x
      y' <- y
      z' <- z
      return $ Fix $ T.Case x' v1 y' v2 z'

class Functor (f Economical) => Opening f by where
  algOpenAt :: by -> Alg f Economical (Int -> Fix (f Economical))

openAt :: Opening f by => Int -> by -> Fix (f Economical) -> Fix (f Economical)
openAt k by ty = cata (algOpenAt by) ty k

open :: Opening f by => by -> Fix (f Economical) -> Fix (f Economical)
open = openAt 0

instance Opening TypeF (Type Economical) where
  algOpenAt by ty k = case ty of
    Unit                  -> Fix Unit
    Var (Bound i)         -> if i == k then by else Fix $ Var $ Bound i
    Var (Free fv)         -> Fix $ Var $ Free fv
    Forall () x           -> Fix $ Forall () $ x $ k + 1
    Arrow x y             -> Fix $ Arrow (x k) (y k)
    Prod x y              -> Fix $ Prod (x k) (y k)
    Sum x y               -> Fix $ Sum (x k) (y k)
    Rec () x              -> Fix $ Rec () $ x $ k + 1
    XType (D () x)        -> Fix $ XType $ D () $ x k
    XType (Suspend eo x)  -> Fix $ XType $ Suspend eo $ x k

instance Opening TypeF (EvalOrder Economical) where
  algOpenAt by ty k = Fix $ case ty of
    Unit                  -> Unit
    Var (Bound i)         -> Var $ Bound i
    Var (Free fv)         -> Var $ Free fv
    Forall () x           -> Forall () $ x k
    Arrow x y             -> Arrow (x k) (y k)
    Prod x y              -> Prod (x k) (y k)
    Sum x y               -> Sum (x k) (y k)
    Rec () x              -> Rec () $ x k
    XType (D () x)        -> XType $ D () $ x $ k + 1
    XType (Suspend eo x)  ->
      XType $ Suspend
        case eo of
          EVar (Bound i) -> if i == k then by else eo
          _              -> eo
        do x k

instance Opening T.TermF (Type Economical) where
  algOpenAt by t k = Fix $ case t of
    T.Unit             -> T.Unit
    T.Var v            -> T.Var v
    T.Abs v x          -> T.Abs v $ x k
    T.App x y          -> T.App (x k) (y k)
    T.Fix v x          -> T.Fix v $ x k
    T.Gen () x         -> T.Gen () $ x $ k + 1
    T.Inst x ty        -> T.Inst (x k) $ openAt k by ty
    T.Ascribe x ty     -> T.Ascribe (x k) $ openAt k by ty
    T.Pair x y         -> T.Pair (x k) (y k)
    T.Proj i x         -> T.Proj i $ x k
    T.Inj i x          -> T.Inj i $ x k
    T.Case x v1 y v2 z -> T.Case (x k) v1 (y k) v2 (z k)

-- Alpha equivalence.
-- Note: don't forget to update this function as the definition of `TypeF` evolves.
equiv :: Has F.Fresh sig m => Type Economical -> Type Economical -> m Bool
equiv (Fix x) (Fix y) = case (x, y) of
  (Unit, Unit) -> return $ True
  (Var v1, Var v2) ->
    case (v1, v2) of
      (Free x, Free y) -> return $ x == y
      _                -> error "unreachable"
  (Forall () x, Forall () y) -> do
    fv :: Type Economical <- Fix . Var . Free <$> freshVar
    open fv x `equiv` open fv y
  (Rec () x, Rec () y) -> do
    fv :: Type Economical <- Fix . Var . Free <$> freshVar
    open fv x `equiv` open fv y
  (Arrow x1 y1, Arrow x2 y2) -> (&&) <$> x1 `equiv` x2 <*> y1 `equiv` y2
  (Prod x1 y1, Prod x2 y2) -> (&&) <$> x1 `equiv` x2 <*> y1 `equiv` y2
  (Sum x1 y1, Sum x2 y2) -> (&&) <$> x1 `equiv` x2 <*> y1 `equiv` y2
  (XType (D () x), XType (D () y)) -> do
    fv :: EvalOrder Economical <- EVar . Free <$> freshVar
    open fv x `equiv` open fv y
  (XType (Suspend eo1 x), XType (Suspend eo2 y)) ->
    (&&)
      case (eo1, eo2) of
        (V, V) -> True
        (N, N) -> True
        (EVar v1, EVar v2) ->
          case (v1, v2) of
            (Free x, Free y) -> x == y
            _                -> error "unreachable"
        _ -> False
      <$> x `equiv` y
  _ -> return False

data TypeError
  = ValueRestriction
  | forall a. CannotSynthesize (T.TermF Economical a)
  | TypeMismatch (Type Economical) (Type Economical)

instance Show TypeError where
  show ValueRestriction     = "value restriction"
  show (CannotSynthesize t) = "cannot synthesize type from this form of expression: " ++ T.getForm t
  show (TypeMismatch x y)   = "type mismatch: " ++ show x ++ " vs " ++ show y

type Effs sig m =
  ( Has (Throw (TypeStructureError Economical)) sig m
  , Has (Throw TypeError) sig m
  , Has F.Fresh sig m
  , Env.Environ Economical sig m
  )

type Alg' f x a = f x (Fix (f x), a) -> a

check :: forall m sig. Effs sig m
          => T.Term Economical -> Type Economical -> m Valueness
check = para $ \t ty -> beforeCheck ty >>= \(ty', f) -> alg t ty' >>= \vn -> f vn
  where
    alg :: Alg' T.TermF Economical (Type Economical -> m Valueness)
    alg t ty = case t of
      T.Unit -> getUnit ty $> Val
      T.Pair (_, x) (_, y) -> do
        (ty1, ty2) <- getProd ty
        vn1 <- x ty1
        vn2 <- y ty2
        return $ vn1 <> vn2
      T.Abs v (_, x) -> do
        (ty1, ty2) <- getArrow ty
        _ <- Env.insert v ty1 $ x ty2
        return Val
      T.Fix v (_, x) -> Env.insertFixpoint v ty (x ty) $> Top
      T.Gen () (t, _) -> do
        ((), ty') <- getForall ty
        fv <- Fix . Var @Economical . Free <$> freshVar
        vn <- check (open fv t) (open fv ty')
        case vn of
          Val -> return Val
          Top -> throwError ValueRestriction
      T.Inj i (_, x) -> do
        (ty1, ty2) <- getSum ty
        x $ T.index i ty1 ty2
      T.Case (t, _) v1 (_, x) v2 (_, y) -> do
        (ty1, ty2) <- synthesize t >>= getSum . fst
        _ <- Env.insert v1 ty1 $ x ty
        _ <- Env.insert v2 ty2 $ y ty
        return Top
      _ -> do
        let t' = Fix $ fst <$> t :: T.Term Economical
        (ty', vn) <- synthesize t'
        b <- equiv ty ty'
        if b
        then return vn
        else throwError $ TypeMismatch ty' ty

beforeCheck :: Has F.Fresh sig m
               => Has (Throw TypeError) sig m
               => Type Economical -> m (Type Economical, Valueness -> m Valueness)
beforeCheck ty =
  case unfix ty of
    Rec () ty'             -> return (open ty ty', return)
    XType (Suspend eo ty') -> return (ty', case eo of {N -> const $ return Val; _ -> return})
    XType (D () ty')       -> do
      fv <- freshVar
      return
        ( open (EVar $ Free fv :: EvalOrder Economical) ty'
        , \case
            Val -> return Val
            _   -> throwError ValueRestriction
        )
    _ -> return (ty, return)

synthesize :: forall m sig. Effs sig m
              => T.Term Economical -> m (Type Economical, Valueness)
synthesize = para $ fmap afterSynth . alg
  where
    alg :: Alg' T.TermF Economical (m (Type Economical, Valueness))
    alg = \case
      T.Ascribe (t, _) ty -> (,) ty <$> check t ty
      T.Var v ->
        second
          \case
            Env.Ordinary -> Val
            Env.Fixpoint -> Top
          <$> Env.lookupVar v
      T.Proj i (_, x) -> x >>= getProd . fst >>= \(ty1, ty2) -> return (T.index i ty1 ty2, Top)
      T.App (_, x) (t, _) -> x >>= getArrow . fst >>= \(ty1, ty2) -> check t ty1 $> (ty2, Top)
      T.Inst (_, x) ty -> do
        (ty', vn) <- x
        ((), ty') <- getForall ty'
        return (open ty ty', vn)
      t -> throwError $ CannotSynthesize t

afterSynth :: (Type Economical, Valueness) -> (Type Economical, Valueness)
afterSynth (ty, vn) =
  case unfix ty of
    XType (Suspend eo ty') ->
      case eo of
        V -> afterSynth (ty', vn)
        _ -> afterSynth (ty', Top)
    Rec () ty' ->
      -- TODO: Ensure contractiveness.
        afterSynth (open ty ty', Top)
    -- D () ty' -> ... -- TODO: Lack of principal types.
    _ -> (ty, vn)

data Errors
  = TS (TypeStructureError Economical)
  | T TypeError
  | E Env.EnvError

instance Show Errors where
  show = \case
    TS e -> show e
    T e  -> show e
    E e  -> show e

runIt :: ThrowC (TypeStructureError Economical) (ThrowC TypeError (F.FreshC (ThrowC Env.EnvError (R.ReaderC (Env.Env Economical) Identity)))) a
    -> Either Env.EnvError (Either TypeError (Either (TypeStructureError Economical) a))
runIt x = run $ R.runReader Env.emptyEnv $ runThrow $ F.evalFresh 0 $ runThrow $ runThrow x

synth :: T.Term Economical -> Either Errors (Type Economical, Valueness)
synth t = either
  do Left . E
  do either
       do Left . T
       do either
            do Left . TS
            do Right
  do runIt $ synthesize t
