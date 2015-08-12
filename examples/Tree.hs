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
treeToBinder Empty = Tagged "Empty" []
treeToBinder (Leaf l) = Tagged "Leaf" [Lit l]
treeToBinder (Branch l r) = Tagged "Branch" [treeToBinder l, treeToBinder r]

binderToTree :: (Eq a) => Binder a -> TreeBinder (Maybe a)
binderToTree (Var Nothing) = NullBinder
binderToTree (Tagged "Empty" []) = Empty
binderToTree (Tagged "Leaf" [Lit l]) = Leaf $ Just l
binderToTree (Tagged "Leaf" [Var _]) = Leaf Nothing
binderToTree (Tagged "Branch" [l, r]) = Branch (binderToTree l) (binderToTree r)
binderToTree _ = error "The given binder is not valid."

env :: String -> [(String, Int)]
env = go
  where
  maps = [("Empty", 0), ("Leaf", 1), ("Branch", 2)]

  go "Empty" = maps
  go "Leaf" = maps
  go "Branch" = maps
  go _ = error "The given name is not a valid constructor."

checkTree :: (Eq a) => [([TreeBinder a], Maybe Guard)] -> ([[TreeBinder (Maybe a)]], [[TreeBinder (Maybe a)]])
checkTree def = (second $ map fromBinder . snd)
               $ (first $ map fromBinder)
               . unwrapCheck
               $ check (makeEnv env) toBinder
  where
  toBinder = map (\(nbs, g) -> (map treeToBinder nbs, g)) def

  fromBinder = map binderToTree
