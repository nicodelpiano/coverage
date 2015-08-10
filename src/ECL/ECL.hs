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

import Data.List (foldl', nub, sortBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Control.Applicative (pure, (<$>), liftA2)

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

-- | Check wraps both uncovered and redundant cases
data Check lit = Check
  { getUncovered :: [Binders lit]
  , getRedundant :: Maybe Bool
  } deriving (Show, Eq) 

makeCheck :: [Binders lit] -> Maybe Bool -> Check lit
makeCheck bs mb = Check
  { getUncovered = bs
  , getRedundant = mb }

unwrapCheck :: Check lit -> ([Binders lit], Maybe Bool)
unwrapCheck ch = (getUncovered ch, getRedundant ch)

applyUncovered :: ([Binders lit] -> [Binders lit]) -> Check lit -> Check lit
applyUncovered f c = makeCheck (f $ getUncovered c) (getRedundant c)

applyRedundant :: (Maybe Bool -> Maybe Bool) -> Check lit -> Check lit
applyRedundant f c = makeCheck (getUncovered c) (f $ getRedundant c)

applyCheck ::
  ([Binders lit] -> [Binders lit]) ->
  (Maybe Bool -> Maybe Bool) ->
  Check lit -> Check lit
applyCheck f g = applyUncovered f . applyRedundant g

-- |
-- Applies a function over two lists of tuples that may lack elements
--
genericMerge :: Ord a =>
  (a -> Maybe b -> Maybe c -> d) ->
  [(a, b)] ->
  [(a, c)] ->
  [d]
genericMerge _ [] [] = []
genericMerge f bs [] = map (\(s, b) -> f s (Just b) Nothing) bs
genericMerge f [] bs = map (\(s, b) -> f s Nothing (Just b)) bs
genericMerge f bsl@((s, b):bs) bsr@((s', b'):bs')
  | s < s' = (f s (Just b) Nothing) : genericMerge f bs bsr
  | s > s' = (f s' Nothing (Just b')) : genericMerge f bsl bs'
  | otherwise = (f s (Just b) (Just b')) : genericMerge f bs bs'

-- | Returns the uncovered set after one binder is applied to the set of
-- values represented by another.
missingSingle :: (Eq lit) => Binder lit -> Binder lit -> (Binders lit, Maybe Bool)
missingSingle _ (Var _) = ([], pure True)
missingSingle b@(Var _) (Tagged tag bc) =
  (map (Tagged tag) $ missed, pure True)
  where
  (missed, _) = missingSingle b bc
missingSingle c@(Tagged tag b) (Tagged tag' b')
  | tag == tag' = let (b'', pr) = missingSingle b b' in (map (Tagged tag) b'', pr)
  | otherwise = ([c], pure False)
missingSingle (Var _) (Record bs) =
  (map (Record . zip names) (getUncovered miss), getRedundant miss)
  where
  miss = missingMultiple (initialize $ length bs) binders

  (names, binders) = unzip bs
missingSingle (Record bs) (Record bs') =
  (map (Record . zip sortedNames) $ getUncovered allMisses, getRedundant allMisses)
  where
  allMisses = uncurry missingMultiple (unzip binders)

  sortNames = sortBy (compare `on` fst)
  
  (sbs, sbs') = (sortNames bs, sortNames bs')

  compB :: a -> Maybe a -> Maybe a -> (a, a)
  compB e b b' = (fm b, fm b')
    where
    fm = fromMaybe e

  compBS :: Eq a => b -> a -> Maybe b -> Maybe b -> (a, (b, b))
  compBS e s b b' = (s, compB e b b')

  (sortedNames, binders) = unzip $ genericMerge (compBS (Var Nothing)) sbs sbs' 
missingSingle b _ = ([b], Nothing) 

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
missingMultiple :: (Eq lit) => Binders lit -> Binders lit -> Check lit
missingMultiple bs bs' = let (unc, red) = go bs bs' in Check { getUncovered = unc, getRedundant = red }
  where
  go [] _ = ([], pure True)
  go (x:xs) (y:ys) = (map (: xs) missed ++ fmap (x :) missed', liftA2 (&&) pr1 pr2)
    where
    (missed, pr1) = missingSingle x y
    (missed', pr2) = go xs ys
  go _ _ = error "Error in missingMultiple: invalid length of argument binders."

-- |
-- `missingCases` applies `missingMultiple` to an alternative
--
missingCases :: (Eq lit) => Binders lit -> Alternative lit -> Check lit
missingCases unc = missingMultiple unc . fst

-- |
-- `missingAlternative` is `missingCases` with guard handling
--
missingAlternative :: (Eq lit) => Alternative lit -> Binders lit -> Check lit
missingAlternative alt unc
  | isExhaustiveGuard $ snd alt = mcases
  | otherwise = Check
    { getUncovered = [unc]
    , getRedundant = getRedundant mcases }
  where
  mcases = missingCases unc alt

  isExhaustiveGuard :: Maybe Guard -> Bool
  isExhaustiveGuard (Just Opaque) = False
  isExhaustiveGuard _ = True

-- |
-- Given a list of alternatives, `check'` generates the proper set of uncovered cases
--
check' :: (Eq lit) => [Alternative lit] -> [Binders lit]
check' cas = getUncovered . applyUncovered nub . foldl' step initial $ cas
  where
  initial = makeCheck [initialize $ length . fst . head $ cas] $ pure True

  step :: (Eq lit) => Check lit -> Alternative lit -> Check lit
  step ch ca =
    let (missed, pr) = unzip $ map (unwrapCheck . missingAlternative ca) $ getUncovered ch
        cond = or <$> sequenceA pr
    in Check
      { getUncovered = concat missed
      , getRedundant = liftA2 (&&) cond $ getRedundant ch }
    where
    sequenceA = foldr (liftA2 (:)) (pure [])

-- |
-- Given two translation functions (between the desired type and `Binder`) and a list of alternatives,
-- `check` generates the proper set of uncovered cases
--
check :: (Eq a, Eq lit) => (a -> Binder lit) -> (Binder lit -> a) -> [([a], Maybe Guard)] -> [[a]]
check toB fromB cas = map fromBs $ check' alt
  where
  alt = map toAlternative cas

  toAlternative (bs, g) = (map toB bs, g)

  fromBs = map fromB
