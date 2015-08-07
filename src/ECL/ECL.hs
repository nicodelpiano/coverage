----------------------------------------
--
-- | Exhaustivity Checking Library (ECL)
--
----------------------------------------
module ECL.ECL
  ( check
  , Literal(..)
  , Binder(..)
  , Guard(..)
  , Uncovered(..)
  ) where

import Data.List (foldl', nub)

-- | Type synonyms: names and lists of binders
type Name = String

-- | A collection of binders, as used to match products, in product binders
-- or collections of binders in top-level declarations.
type Binders = [Binder]

-- |
-- Literals
--
data Literal
  = NumLit Int -- NumLit (Either Int Double)
  | StrLit String
  | CharLit Char
  | BoolLit Bool
  deriving (Show, Eq)

-- |
-- Binders
--
data Binder
  = Var Name
  | Lit Literal
  | Tagged Name Binder
  | Product Binders
  | Record [(Name, Binder)]
  deriving (Show, Eq)

-- | May be useful some of these defs
boolean :: Bool -> Binder
boolean = Lit . BoolLit

-- | Guards and alternatives
-- 
-- Guard are abstract, and it is up to the language implementor to interpret
-- guards abstractly. Guards can catch all cases, or represent some opaque
-- expression which cannot be analysed.
data Guard = CatchAll | Opaque
  deriving (Show, Eq)

-- | A case alternative consists of a collection of binders which match
-- a collection of values, and an optional guard.
type Alternative = (Binders, Maybe Guard)

-- | A list of uncovered cases
newtype Uncovered = Uncovered { getUncovered :: [Binders] }
  deriving (Show, Eq) 

uncovered :: [Binders] -> Uncovered
uncovered = Uncovered

applyUncovered :: ([Binders] -> [Binders]) -> Uncovered -> Uncovered
applyUncovered f = uncovered . f . getUncovered

-- |
-- `missingSingle` returns the uncovered set between two binders
--
missingSingle :: Binder -> Binder -> Binders
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
missingSingle b@(Var _) (Lit l) = missingSingleLit b l
missingSingle b@(Lit _) (Lit l) = missingSingleLit b l
missingSingle b _ = [b] -- incomplete

missingSingleLit :: Binder -> Literal -> Binders
missingSingleLit (Var _) (BoolLit b) = [boolean $ not b]
missingSingleLit b@(Lit (BoolLit bl)) (BoolLit br)
  | bl == br = []
  | otherwise = [b]
missingSingleLit b _ = [b]

-- |
-- Generates a list of initial binders
--
initialize :: Int -> Binders
initialize = flip replicate $ wildcard
  where
  wildcard = Var "_"

-- |
-- `missingMultiple` returns the whole set of uncovered cases
--
missingMultiple :: Binders -> Binders -> Uncovered
missingMultiple bs = uncovered . go bs
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
missingCases :: Binders -> Alternative -> Uncovered
missingCases unc = missingMultiple unc . fst

-- |
-- `missingAlternative` is `missingCases` with guard handling
--
missingAlternative :: Alternative -> Binders -> [Binders]
missingAlternative alt unc
  | isExhaustiveGuard $ snd alt = getUncovered mcases
  | otherwise = [unc]
  where
  mcases = missingCases unc alt

  isExhaustiveGuard :: Maybe Guard -> Bool
  isExhaustiveGuard (Just Opaque) = True
  isExhaustiveGuard _ = True

-- |
-- Given a list of alternatives, `check` generates the proper set of uncovered cases
--
check :: [Alternative] -> Uncovered
check cas = applyUncovered nub $ foldl' step (Uncovered [initial]) cas
  where
  initial = initialize $ length . fst . head $ cas

  step :: Uncovered -> Alternative -> Uncovered
  step unc ca = applyUncovered (concatMap (missingAlternative ca)) unc
