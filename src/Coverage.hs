-----------------------------------------------------------------------------
--
-- Module      :  Coverage
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Exhaustivity checking main function.
--
-----------------------------------------------------------------------------

module Coverage
  ( check
  , Binder(..)
  , Guard(..)
  , makeEnv
  , Check(..)
  , Redundant(..)
  ) where

import Coverage.Internal

import Data.List (foldl', nub)
import Data.Maybe (fromMaybe)

import Control.Applicative ((<$>), liftA2)

-- |
-- Given a list of alternatives, `check` generates the proper set of uncovered cases.
--
check :: (Eq lit) => Environment -> [Alternative lit] -> Check lit
check env cas = applyRedundant (fmap nub) . applyUncovered nub . foldl' step initial $ cas
  where
  initial = makeCheck [initialize $ length . fst . head $ cas] $ Redundant []

  step :: (Eq lit) => Check lit -> Alternative lit -> Check lit
  step ch ca =
    let (missed, pr) = unzip $ map (missingAlternative env ca) $ getUncovered ch
        cond = liftA2 (&&) (or <$> sequence pr) $ mr . getRedundant $ ch
    in applyRedundant (\_ -> if fromMaybe True cond then getRedundant ch else fmap (fst ca :) $ getRedundant ch)
       . applyUncovered (\unc -> if fromMaybe True cond then concat missed else unc) $ ch
      where
      mr :: Redundant [Binders lit] -> Maybe Bool
      mr DontKnow = Nothing
      mr NotRedundant = Just False
      mr (Redundant _) = Just True
