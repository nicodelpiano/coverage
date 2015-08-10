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

natToBinder :: NatBinder -> Binder NatBinder
natToBinder NullBinder = Var Nothing
natToBinder Zero = Tagged "Zero" $ Var Nothing
natToBinder (Succ nb) = Tagged "Succ" $ natToBinder nb

binderToNat :: Binder NatBinder -> NatBinder
binderToNat (Var Nothing) = NullBinder
binderToNat (Tagged "Zero" (Var Nothing)) = Zero
binderToNat (Tagged "Succ" b) = Succ $ binderToNat b
binderToNat _ = error "This should not be happening."

env :: Environment
env =
  [ ("Zero", 0)
  , ("Succ", 1)]

checkNat :: [([NatBinder], Maybe Guard)] -> ([[NatBinder]], [[NatBinder]])
checkNat = check env natToBinder binderToNat
