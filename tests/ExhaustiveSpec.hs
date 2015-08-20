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
  describe "Properties" $ do
    describe "missingSingle" $ do
      it "between any binder and `Var _` should be `[]`" $
        property $ \b n -> fst (missingSingle defaultEnv (b :: Binder String) (Var n)) == []
      it "between the same binders should be `[]`" $
        property $ \b -> fst (missingSingle defaultEnv (b :: Binder String) b) == []

main :: IO ()
main = hspec spec
