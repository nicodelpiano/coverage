-----------------------------------------------------------------------------
--
-- Module      :  CoverageSpec
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Tests.
--
-----------------------------------------------------------------------------

module CoverageSpec (main, spec) where

import Control.Coverage
import Control.Coverage.Internal
import CoverageSupport

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
