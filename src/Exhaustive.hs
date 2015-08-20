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
  ) where

import Exhaustive.Internal

import Data.List (foldl', nub)
import Data.Maybe (fromMaybe)

import Control.Applicative (pure, (<$>), liftA2)

-- |
-- Given a list of alternatives, `check` generates the proper set of uncovered cases.
--
check :: (Eq lit) => Environment -> [Alternative lit] -> Check lit
check env cas = applyUncovered nub . foldl' step initial $ cas
  where
  initial = makeCheck [initialize $ length . fst . head $ cas] $ NotRedundant []

  step :: (Eq lit) => Check lit -> Alternative lit -> Check lit
  step ch ca =
    let (missed, pr) = unzip $ map (missingAlternative env ca) $ getUncovered ch
        cond = liftA2 (&&) (or <$> sequence pr) $ mr . getRedundant $ ch
    in applyRedundant (\_ -> if fromMaybe True cond then getRedundant ch else fmap ((:) (fst ca)) $ getRedundant ch)
       . applyUncovered (\unc -> if fromMaybe True cond then concat missed else unc) $ ch
      where
      mr :: Redundant [Binders lit] -> Maybe Bool
      mr DontKnow = Nothing
      mr Redundant = Just False
      mr (NotRedundant _) = Just True
