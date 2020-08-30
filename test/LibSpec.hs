{-# LANGUAGE TypeFamilies #-}

module LibSpec where

import Test.Hspec

import Data.Bifunctor
import Data.Either

import Control.Carrier.Fresh.Strict
import Data.Functor.Foldable

import Economical
import EvalOrder
import Lib
import Type

newtype EType = EType (Type Economical)
  deriving Show

instance Eq EType where
  EType x == EType y = run $ evalFresh 0 $ equiv x y

fromString' :: String -> Either String (EType, Valueness)
fromString' = second (first EType) . fromString

result :: (Type Economical, a) -> Either e (EType, a)
result = Right . first EType

bound :: Int -> Fix (TypeF Economical)
bound n = Fix $ Var $ Bound n

spec :: Spec
spec = do
  describe "integration" $
    it "test" $ do
      fromString' "() : 1" `shouldBe` result (Fix Unit, Val)
      fromString' "(Λ'b -> fun x -> x) : ∀'a. 'a -> 'a" `shouldBe` result (Fix $ Forall () $ Fix $ Arrow (bound 0) (bound 0), Val)
      fromString' "(Λ'a -> λx -> match x with nil -> inl nil | p -> snd p end) : Д'A. ∀'a. (μ'b. 'A |> (1 + 'a * 'b)) -> (μ'b. 'A |> (1 + 'a * 'b))" `shouldSatisfy` isRight

      fromString' "()" `shouldSatisfy` isLeft
