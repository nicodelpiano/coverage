module ExhaustiveSpec (main, spec) where

import Exhaustive
import Exhaustive.Internal
import ExhaustiveSupport

import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "Test suite" $ do
    describe "missingSingle" $ do
      it "between any binder and `Var _` should be `[]`" $
        property $ \b n -> fst (missingSingle defaultEnv (b :: Binder String) (Var n)) == []

      it "between the same binders should be `[]`" $
        property $ \b -> fst (missingSingle defaultEnv (b :: Binder String) b) == []

      it "if we covered something, then uncovered should be `[]`" $
        property $ \b b' ->
          let (unc, cov) = missingSingle defaultEnv (b :: Binder String) (b' :: Binder String)
          in cov == Just True ==> unc == []

      it "if there are no uncovered cases, then we covered something" $
        property $ \b b' ->
          let (unc, cov) = missingSingle defaultEnv (b :: Binder String) (b' :: Binder String)
          in unc == [] ==> cov == Just True

main :: IO ()
main = hspec spec
