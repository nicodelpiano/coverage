module Nat where

import ECL.ECL

import Control.Arrow (first, second)

-- | Nat
data Nat = Z | S Nat
  deriving (Show, Eq)

-- | Name binding association
data NatBinder = NullBinder       -- Wildcard binder
               | Zero             -- Zero binder
               | Succ NatBinder   -- Succ binder
  deriving (Show, Eq)

natToBinder :: NatBinder -> Binder NatBinder
natToBinder NullBinder = Var Nothing
natToBinder Zero = Tagged "Zero" $ Var Nothing
natToBinder (Succ nb) = Tagged "Succ" $ natToBinder nb

binderToNat :: Binder NatBinder -> NatBinder
binderToNat (Var Nothing) = NullBinder
binderToNat (Tagged "Zero" (Var Nothing)) = Zero
binderToNat (Tagged "Succ" b) = Succ $ binderToNat b
binderToNat _ = error "The given binder is not valid."

env :: String -> [(String, Int)]
env "Zero" =
  [ ("Zero", 0)
  , ("Succ", 1)]
env "Succ" =
  [ ("Zero", 0)
  , ("Succ", 1)]
env _ = error "The given name is not a valid constructor."

checkNat :: [([NatBinder], Maybe Guard)] -> ([[NatBinder]], [[NatBinder]])
checkNat def = (second $ map fromBinder . snd)
               $ (first $ map fromBinder)
               . unwrapCheck
               $ check (makeEnv env) toBinder
  where
  toBinder = map (\(nbs, g) -> (map natToBinder nbs, g)) def

  fromBinder = map binderToNat
