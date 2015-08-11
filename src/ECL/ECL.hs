----------------------------------------
--
-- | Exhaustivity Checking Library (ECL)
--
----------------------------------------
module ECL.ECL
  ( check
  , Binder(..)
  , Guard(..)
  , makeEnv
  , unwrapCheck
  ) where

import Data.List (foldl', nub, sortBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Control.Applicative (pure, (<$>), liftA2)

-- | A type synonym for names. 
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

-- | Represents constructors' arities.
type Arity = Int

-- | Environment
--
-- The language implementor should provide an environment to let
-- the checker lookup for constructors' names (just for one type for now).
newtype Environment = Environment { envInfo :: Name -> [(Name, Arity)] }

makeEnv :: (Name -> [(Name, Arity)]) -> Environment
makeEnv info = Environment { envInfo = info }

defaultEnv :: Environment
defaultEnv = Environment { envInfo = (\_ -> []) }

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

-- | Check wraps both uncovered and redundant cases.
data Check lit = Check
  { getUncovered :: [Binders lit]
  , getRedundant :: (Maybe Bool, [Binders lit])
  } deriving (Show, Eq) 

makeCheck :: [Binders lit] -> (Maybe Bool, [Binders lit]) -> Check lit
makeCheck bs red = Check
  { getUncovered = bs
  , getRedundant = red }

unwrapCheck :: Check lit -> ([Binders lit], (Maybe Bool, [Binders lit]))
unwrapCheck ch = (getUncovered ch, getRedundant ch)

applyUncovered :: ([Binders lit] -> [Binders lit]) -> Check lit -> Check lit
applyUncovered f c = makeCheck (f $ getUncovered c) (getRedundant c)

applyRedundant :: ((Maybe Bool, [Binders lit]) -> (Maybe Bool, [Binders lit])) -> Check lit -> Check lit
applyRedundant f c = makeCheck (getUncovered c) (f $ getRedundant c)

-- | Wildcard
wildcard :: Binder lit
wildcard = Var Nothing

-- |
-- Applies a function over two lists of tuples that may lack elements.
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
missingSingle :: (Eq lit) => Environment -> Binder lit -> Binder lit -> ([Binder lit], Maybe Bool)
missingSingle _ _ (Var _) = ([], pure True)
missingSingle env (Var _) cb@(Tagged con _) =
  (concatMap (\cp -> fst $ missingSingle env cp cb) $ tagEnv, pure True)
  where
  -- for now, we have arity 1
  tag :: (Eq lit) => (Name, Arity) -> Binder lit
  tag (n, _) = Tagged n wildcard

  tagEnv :: (Eq lit) => [Binder lit]
  tagEnv = foldr (\x xs -> tag x : xs) [] $ envInfo env $ con
missingSingle env c@(Tagged tag b) (Tagged tag' b')
  | tag == tag' = let (b'', pr) = missingSingle env b b' in (map (Tagged tag) b'', pr)
  | otherwise = ([c], pure False)
missingSingle env (Var _) (Record bs) =
  (map (Record . zip names) $ miss, pr)
  where
  (miss, pr) = missingMultiple env (initialize $ length bs) binders

  (names, binders) = unzip bs
missingSingle env (Record bs) (Record bs') =
  (map (Record . zip sortedNames) $ allMisses, pr)
  where
  (allMisses, pr) = uncurry (missingMultiple env) $ unzip binders

  sortNames = sortBy (compare `on` fst)
  
  (sbs, sbs') = (sortNames bs, sortNames bs')

  compB :: a -> Maybe a -> Maybe a -> (a, a)
  compB e b b' = (fm b, fm b')
    where
    fm = fromMaybe e

  compBS :: Eq a => b -> a -> Maybe b -> Maybe b -> (a, (b, b))
  compBS e s b b' = (s, compB e b b')

  (sortedNames, binders) = unzip $ genericMerge (compBS (Var Nothing)) sbs sbs' 
missingSingle _ b _ = ([b], Nothing) 

-- |
-- Generates a list of initial binders.
--
initialize :: Int -> [Binder lit]
initialize = flip replicate $ wildcard

-- |
-- `missingMultiple` returns the whole set of uncovered cases.
--
missingMultiple :: (Eq lit) => Environment -> Binders lit -> Binders lit -> ([Binders lit], Maybe Bool)
missingMultiple env = go
  where
  go [] _ = ([], pure True)
  go (x:xs) (y:ys) = (map (: xs) missed ++ fmap (x :) missed', liftA2 (&&) pr1 pr2)
    where
    (missed, pr1) = missingSingle env x y
    (missed', pr2) = go xs ys
  go _ _ = error "Error in missingMultiple: invalid length of argument binders."

-- |
-- `missingCases` applies `missingMultiple` to an alternative.
--
missingCases :: (Eq lit) => Environment -> Binders lit -> Alternative lit -> ([Binders lit], Maybe Bool)
missingCases env unc = missingMultiple env unc . fst

-- |
-- `missingAlternative` is `missingCases` with guard handling.
--
missingAlternative :: (Eq lit) => Environment -> Alternative lit -> Binders lit -> ([Binders lit], Maybe Bool)
missingAlternative env alt unc
  | isExhaustiveGuard $ snd alt = mcases
  | otherwise = ([unc], snd mcases)
  where
  mcases = missingCases env unc alt

  isExhaustiveGuard :: Maybe Guard -> Bool
  isExhaustiveGuard (Just Opaque) = False
  isExhaustiveGuard _ = True

-- |
-- Given a list of alternatives, `check` generates the proper set of uncovered cases.
--
check :: (Eq lit) => Environment -> [Alternative lit] -> Check lit
check env cas = applyUncovered nub . foldl' step initial $ cas
  where
  initial = makeCheck [initialize $ length . fst . head $ cas] $ (pure True, [])

  step :: (Eq lit) => Check lit -> Alternative lit -> Check lit
  step ch ca =
    let (missed, pr) = unzip $ map (missingAlternative env ca) $ getUncovered ch
        (mb, redundant) = getRedundant ch
        cond = liftA2 (&&) (or <$> sequence pr) mb
    in applyRedundant (\_ -> (cond, if fromMaybe True cond then redundant else fst ca : redundant))
       . applyUncovered (\unc -> if fromMaybe True cond then concat missed else unc) $ ch
