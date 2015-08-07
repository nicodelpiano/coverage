----------------------------------------
--
-- | Exhaustivity Checking Library (ECL)
--
----------------------------------------
module ECL.ECL
  ( check
  , Binder(..)
  , Guard(..)
  ) where

import Data.List (foldl', nub)

-- | Type synonyms: names and lists of binders
type Name = String

-- | A collection of binders, as used to match products, in product binders
-- or collections of binders in top-level declarations.
type Binders lit = [Binder lit]

-- |
-- Binders
--
data Binder lit
  = Var (Maybe Name)
  | Lit lit
  | Tagged Name (Binder lit)
  | Product (Binders lit)
  | Record [(Name, Binder lit)]
  deriving (Show, Eq)

-- | Guards and alternatives
-- 
-- Guard are abstract, and it is up to the language implementor to interpret
-- guards abstractly. Guards can catch all cases, or represent some opaque
-- expression which cannot be analysed.
data Guard = CatchAll | Opaque
  deriving (Show, Eq)

-- | A case alternative consists of a collection of binders which match
-- a collection of values, and an optional guard.
type Alternative lit = (Binders lit, Maybe Guard)

-- | A list of uncovered cases
newtype Uncovered lit = Uncovered { getUncovered :: [Binders lit] }
  deriving (Show, Eq) 

applyUncovered :: ([Binders lit] -> [Binders lit]) -> Uncovered lit -> Uncovered lit
applyUncovered f = Uncovered . f . getUncovered

-- | Returns the uncovered set after one binder is applied to the set of
-- values represented by another.
missingSingle :: Binder lit -> Binder lit -> Binders lit
missingSingle _ (Var _) = []
missingSingle b@(Var _) (Tagged tag bc) =
  map (Tagged tag) $ missingSingle b bc
missingSingle c@(Tagged tag _) (Tagged tag' _)
  | tag == tag' = []
  | otherwise = [c]
missingSingle (Var _) (Record bs) =
  map (Record . zip names) miss
  where
  miss = getUncovered $ missingMultiple (initialize $ length bs) binders
  (names, binders) = unzip bs
missingSingle b _ = [b] 

-- |
-- Generates a list of initial binders
--
initialize :: Int -> Binders lit
initialize = flip replicate $ wildcard
  where
  wildcard = Var Nothing

-- |
-- `missingMultiple` returns the whole set of uncovered cases
--
missingMultiple :: Binders lit -> Binders lit -> Uncovered lit
missingMultiple bs = Uncovered . go bs
  where
  go [] _ = []
  go (x:xs) (y:ys) = map (: xs) missed ++ fmap (x :) missed'
    where
    missed = missingSingle x y
    missed' = go xs ys
  go _ _ = error "Error in missingMultiple: invalid length of argument binders."

-- |
-- `missingCases` applies `missingMultiple` to an alternative
--
missingCases :: Binders lit -> Alternative lit -> Uncovered lit
missingCases unc = missingMultiple unc . fst

-- |
-- `missingAlternative` is `missingCases` with guard handling
--
missingAlternative :: Alternative lit -> Binders lit -> [Binders lit]
missingAlternative alt unc
  | isExhaustiveGuard $ snd alt = getUncovered mcases
  | otherwise = [unc]
  where
  mcases = missingCases unc alt

  isExhaustiveGuard :: Maybe Guard -> Bool
  isExhaustiveGuard (Just Opaque) = False
  isExhaustiveGuard _ = True

-- |
-- Given a list of alternatives, `check` generates the proper set of uncovered cases
--
check :: (Eq lit) => [Alternative lit] -> [Binders lit]
check cas = getUncovered . applyUncovered nub . foldl' step (Uncovered [initial]) $ cas
  where
  initial = initialize $ length . fst . head $ cas

  step :: Uncovered lit -> Alternative lit -> Uncovered lit
  step unc ca = applyUncovered (concatMap (missingAlternative ca)) unc
