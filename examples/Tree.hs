module Tree where

import ECL.ECL

import Control.Arrow (first, second)

-- | Tree data-type
data Tree a = E
            | L a
            | B (Tree a) (Tree a)
  deriving (Show, Eq)

-- | Tree binders
data TreeBinder a = NullBinder
                  | Empty
                  | Leaf a
                  | Branch (TreeBinder a) (TreeBinder a)
  deriving (Show, Eq)

treeToBinder :: (Eq a) => TreeBinder a -> Binder a
treeToBinder NullBinder = Var Nothing
treeToBinder Empty = Tagged "Empty" $ Var Nothing
treeToBinder (Leaf l) = Tagged "Leaf" $ Lit l
treeToBinder (Branch l r) = Tagged "Branch" $ Product [treeToBinder l, treeToBinder r]

binderToTree :: (Eq a) => Binder a -> TreeBinder (Maybe a)
binderToTree (Var Nothing) = NullBinder
binderToTree (Tagged "Empty" _) = Empty
binderToTree (Tagged "Leaf" (Lit l)) = Leaf $ Just l
binderToTree (Tagged "Leaf" (Var _)) = Leaf Nothing
binderToTree (Tagged "Branch" (Var _)) = Branch NullBinder NullBinder
binderToTree (Tagged "Branch" (Product [l, r])) = Branch (binderToTree l) (binderToTree r)
binderToTree _ = error "The given binder is not valid."

env :: String -> Maybe [(String, Int)]
env = go
  where
  maps = Just [("Empty", 0), ("Leaf", 1), ("Branch", 2)]

  go "Empty" = maps
  go "Leaf" = maps
  go "Branch" = maps
  go _ = error "The given name is not a valid constructor."

checkTree :: (Eq a) => [([TreeBinder a], Maybe Guard)] -> ([[TreeBinder (Maybe a)]], [[TreeBinder (Maybe a)]])
checkTree def =
  let ch = check (makeEnv env) toBinder
  in (map fromBinder $ getUncovered ch, map fromBinder . snd $ getRedundant ch)
  where
  toBinder = map (\(nbs, g) -> (map treeToBinder nbs, g)) def

  fromBinder = map binderToTree
