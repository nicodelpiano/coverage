----------------------------------------
--
-- | Exhaustivity Checking Library (ECL)
--
----------------------------------------
module Exhaustive
  ( check
  , Binder(..)
  , Guard(..)
  , makeEnv
  , Check(..)
  , fromRedundant
  , Redundant(..)
  ) where

import Exhaustive.Internal

import Data.List (foldl', nub)

import Control.Applicative (pure, (<$>), liftA2)

-- |
-- Given a list of alternatives, `check` generates the proper set of uncovered cases.
--
check :: (Eq lit) => Environment -> [Alternative lit] -> Check lit
check env cas = applyUncovered nub . foldl' step initial $ cas
  where
  initial = makeCheck [initialize $ length . fst . head $ cas] $ pure []

  step :: (Eq lit) => Check lit -> Alternative lit -> Check lit
  step ch ca =
    let (missed, pr) = unzip $ map (missingAlternative env ca) $ getUncovered ch
        cond = liftA2 mr ((elem ()) <$> sequence pr) $ getRedundant ch
    in applyRedundant (\_ -> if not $ f cond then (fst ca :) <$> getRedundant ch else getRedundant ch)
       . applyUncovered (\unc -> if cond == Redundant then unc else concat missed)
       $ ch
      where
      mr False redundant = False
      mr _ _ = True

      f Redundant = False
      f _ = True
