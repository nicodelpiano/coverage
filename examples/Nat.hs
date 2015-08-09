module Nat where

import ECL.ECL

-- | Nat
data Nat = Z | S Nat
  deriving (Show, Eq)

-- | Name binding association
data NatBinder = NullBinder       -- Wildcard binder
               | Zero             -- Zero binder
               | Succ NatBinder   -- Succ binder
  deriving (Show, Eq)

natToBinder :: (Eq a) => NatBinder -> Binder a
natToBinder NullBinder = Var Nothing
natToBinder Zero = Tagged "Zero" $ Var Nothing
natToBinder (Succ nb) = Tagged "Succ" $ natToBinder nb

binderToNat :: (Eq a) => Binder a -> NatBinder
binderToNat (Var Nothing) = NullBinder
binderToNat (Tagged "Zero" (Var Nothing)) = Zero
binderToNat (Tagged "Succ" b) = Succ $ binderToNat b
binderToNat _ = error "This should not be happening."

checkNat :: [([NatBinder], Maybe Guard)] -> [[NatBinder]]
checkNat = (check :: ((NatBinder -> Binder NatBinder) -> (Binder NatBinder -> NatBinder) -> [([NatBinder], Maybe Guard)] -> [[NatBinder]])) natToBinder binderToNat
