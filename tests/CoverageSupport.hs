-----------------------------------------------------------------------------
--
-- Module      :  CoverageSupport
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- QuickCheck instance for Binder data-type.
--
-----------------------------------------------------------------------------
   
module CoverageSupport where

import Coverage.Internal

import Control.Monad (liftM, liftM2, liftM3)

import Test.QuickCheck

-- |
-- Generates a list of binders.
--
genBinders :: (Eq a, Arbitrary a) => Int -> Gen (Binders a)
genBinders 0 = return []
genBinders n = liftM2 (:) arbitrary (genBinders (n-1))

-- |
-- Generates a list of pairs (name, binder).
--
genRec :: (Eq a, Arbitrary a) => Int -> Gen [(Name, Binder a)]
genRec 0 = return []
genRec n = liftM3 (\n b -> (:) (n, b)) arbitrary arbitrary (genRec (n-1))

-- |
-- Specific generators for each type of binder.
--
genVar :: (Eq a, Arbitrary a) => Gen (Binder a)
genVar = liftM Var arbitrary

-- |
-- Although necessary, there are no tests for tagged yet since
-- we need to construct an specific environment.
--
genTagged :: (Eq a, Arbitrary a) => Gen (Binder a)
genTagged = liftM2 Tagged arbitrary arbitrary

genLit :: (Eq a, Arbitrary a) => Gen (Binder a)
genLit = liftM Lit arbitrary

genRecord :: (Eq a, Arbitrary a) => Gen (Binder a)
genRecord = liftM Record (choose (0, 2) >>= genRec)

genProduct :: (Eq a, Arbitrary a) => Gen (Binder a)
genProduct = liftM Product (choose (0, 2) >>= genBinders)

instance (Eq a, Arbitrary a) => Arbitrary (Binder a) where
  arbitrary = oneof
    [ genVar
    , genLit
    , genProduct
    , genRecord
    ]
