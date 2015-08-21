-----------------------------------------------------------------------------
--
-- Module      :  Coverage.Internal
-- Copyright   :  (c) 2015 Nicolas Del Piano
-- License     :  MIT
--
-- Maintainer  :  Nicolas Del Piano <ndel314@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Module for internal representation.
--
-----------------------------------------------------------------------------

module Coverage.Internal where

import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Control.Applicative (Applicative, (<*>), pure, liftA2)
import Control.Arrow (first)

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

-- | Environment
--
-- The language implementor should provide an environment to let
-- the checker lookup for constructors' names (just for one type for now).
newtype Environment = Environment { envInfo :: Name -> Maybe [Name] }

makeEnv :: (Name -> Maybe [Name]) -> Environment
makeEnv info = Environment { envInfo = info }

defaultEnv :: Environment
defaultEnv = makeEnv (\_ -> Nothing)

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

-- | Data-type for redundant cases representation.
data Redundant a = DontKnow | NotRedundant | Redundant a
  deriving (Show, Eq)

-- | Functor instance for Redundant (TODO: proofs).
instance Functor Redundant where
  fmap _ DontKnow      = DontKnow
  fmap _ NotRedundant  = NotRedundant
  fmap f (Redundant r) = Redundant $ f r

-- | Applicative instance for Redundant.
instance Applicative Redundant where
  pure = Redundant

  DontKnow <*> _      = DontKnow
  NotRedundant <*> _  = NotRedundant
  (Redundant f) <*> m = fmap f m

-- | Check wraps both uncovered and redundant cases.
data Check lit = Check
  { getUncovered :: [Binders lit]
  , getRedundant :: Redundant [Binders lit]
  } deriving (Show, Eq) 

makeCheck :: [Binders lit] -> Redundant [Binders lit] -> Check lit
makeCheck bs red = Check
  { getUncovered = bs
  , getRedundant = red }

applyUncovered :: ([Binders lit] -> [Binders lit]) -> Check lit -> Check lit
applyUncovered f c = makeCheck (f $ getUncovered c) (getRedundant c)

applyRedundant :: (Redundant [Binders lit] -> Redundant [Binders lit]) -> Check lit -> Check lit
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
missingSingle env (Var _) (Product ps) =
  first (map Product) $ missingMultiple env (initialize $ length ps) ps
missingSingle env p@(Product ps) (Product ps')
  | length ps == length ps' = first (map Product) $ missingMultiple env ps ps'
  | otherwise = ([p], pure False)
missingSingle env (Var _) cb@(Tagged con _) =
  (concatMap (\cp -> fst $ missingSingle env cp cb) $ tagEnv, pure True)
  where
  tag :: (Eq lit) => Name -> Binder lit
  tag n = Tagged n wildcard

  tagEnv :: (Eq lit) => [Binder lit]
  tagEnv = map tag
           . fromMaybe (error $ "Constructor name '" ++ con ++ "' not in the scope of the current environment in missingSingle.")
           . envInfo env $ con
missingSingle env c@(Tagged tag bs) (Tagged tag' bs')
  | tag == tag' = let (bs'', pr) = missingSingle env bs bs' in (map (Tagged tag) bs'', pr)
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
missingSingle _ b@(Lit l) (Lit l')
  | l == l' = ([], pure True)
  | otherwise = ([b], pure False) 
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
  go [] [] = ([], pure True)
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
