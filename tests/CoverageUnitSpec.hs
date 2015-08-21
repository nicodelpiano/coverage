-----------------------------------------------------------------------------
--
-- Module      :  CoverageUnitSpec
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Unit tests.
--
-----------------------------------------------------------------------------

module CoverageUnitSpec (main, spec) where

import Coverage
import Coverage.Internal
import CoverageSupport

import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import Control.Exception (evaluate)

env :: Environment
env = makeEnv env'
  where
  env' :: String -> Maybe [String]
  env' "Zero" = Just $
    ["Zero", "Succ"]
  env' "Succ" = Just $
    ["Zero", "Succ"]
  env' _ = error "The given name is not a valid constructor."

z :: Binder ()
z = Tagged "Zero" wildcard

s :: Binder () -> Binder ()
s b = Tagged "Succ" b

tagged_def1 :: [Alternative ()]
tagged_def1 = [([z], Nothing)]

tagged_def2 :: [Alternative ()]
tagged_def2 = [([z], Nothing),([s wildcard], Nothing)]

tagged_def3 :: [Alternative ()]
tagged_def3 = [([z, z], Nothing)]

tagged_def4 :: [Alternative ()]
tagged_def4 = [([z, z], Nothing), ([wildcard, wildcard], Nothing)]

tagged_def5 :: [Alternative ()]
tagged_def5 = [([wildcard], Nothing), ([z], Nothing)]

tagged_def6 :: [Alternative ()]
tagged_def6 = [([wildcard, wildcard, wildcard], Nothing), ([z, z, z], Nothing)]

fromRedundant (Redundant bs) = bs
fromRedundant _ = []

lit_def1 :: [Alternative String]
lit_def1 = [([Lit "hello"], Nothing)]

record_def1 :: [Alternative ()]
record_def1 = [([Record [("foo", wildcard)]], Nothing)]

record_def2 :: [Alternative ()]
record_def2 = [([Record [("foo", z)]], Nothing), ([Record [("bar", z), ("foo", s z)]], Nothing)]

product_def1 :: [Alternative ()]
product_def1 = [([Product []], Nothing)]

product_def2 :: [Alternative ()]
product_def2 = [([Product [z, z]], Nothing)]

spec :: Spec
spec = do
  describe "Unit tests" $ do

    describe "Literals" $ do
      it "lit_def1 is not exhaustive" $ do
        (getUncovered $ check defaultEnv lit_def1) `shouldBe` [[wildcard]]

      it "lit_def1 has redundant cases" $ do
        (getUncovered $ check defaultEnv lit_def1) `shouldBe` [[wildcard]]

    describe "Tagged" $ do
      it "tagged_def1 is not exhaustive" $ do
        (getUncovered $ check env tagged_def1) `shouldBe` [[s wildcard]]

      it "tagged_def1 has not redundant cases" $ do
        (fromRedundant $ getRedundant $ check env tagged_def1) `shouldBe` []

      it "tagged_def2 is exhaustive" $ do
        (getUncovered $ check env tagged_def2) `shouldBe` []

      it "tagged_def3 is not exhaustive" $ do
        (getUncovered $ check env tagged_def3) `shouldBe` [[s wildcard, wildcard], [wildcard, s wildcard]]

      it "tagged_def4 is exhaustive" $ do
        (getUncovered $ check env tagged_def4) `shouldBe` []

      it "tagged_def5 has redundant cases" $ do
        (fromRedundant $ getRedundant $ check env tagged_def5) `shouldBe` [[z]]

      it "tagged_def6 has redundant cases" $ do
        (fromRedundant $ getRedundant $ check env tagged_def6) `shouldBe` [[z, z, z]]

    describe "Record" $ do
      it "record_def1 is exhaustive" $ do
        (getUncovered $ check env record_def1) `shouldBe` []
 
      it "record_def2 is not exhaustive" $ do
        (getUncovered $ check env record_def2) `shouldBe` [[Record [("bar", s wildcard), ("foo", s wildcard)]], [Record [("bar", wildcard),("foo", s $ s wildcard)]]]

    describe "Product" $ do
      it "product_def1 is exhaustive" $ do
        (getUncovered $ check env record_def1) `shouldBe` []

      it "product_def2 is not exhaustive" $ do
        (getUncovered $ check env record_def2) `shouldBe` [[Record [("bar", s wildcard), ("foo", s wildcard)]], [Record [("bar", wildcard), ("foo", s $ s wildcard)]]]

main :: IO ()
main = hspec spec
