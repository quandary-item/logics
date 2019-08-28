module Unification where

import qualified Data.Map as Map

import Logic

-- given the antecendent or consequent of a nat deduction rule,
-- try to match it with a given formula and return what values must be mapped to
-- make them match

-- e.g.
-- A, P, [] -> [A -> P]
-- A, P, [A -> Q] -> Nothing

unify' :: (Ord t, Eq t') => Logic t -> Logic t' -> Maybe (Map.Map t (Logic t'))
unify' l r = unify l r Map.empty

unify :: (Ord t, Eq t') => Logic t -> Logic t' -> Map.Map t (Logic t') -> Maybe (Map.Map t (Logic t'))
unify (Val a) p assignment = case lookupRes of
  Just v -> if v == p then (Just $ Map.insert a p assignment) else Nothing
  _      -> Just $ Map.insert a p assignment
  where
    lookupRes = Map.lookup a assignment

unify (Un unOp a) (Un unOp' b) assignment
  | unOp == unOp' = unify a b assignment
  | otherwise     = Nothing

unify (Un _ _) _ _ = Nothing

-- TODO: redo using do-notation
unify (Bin binOp l r) (Bin binOp' l' r') assignment
  | binOp == binOp' = do
      assignment'  <- unify l l' assignment
      assignment'' <- unify r r' assignment'
      pure assignment''
  | otherwise = Nothing

unify (Bin _ _ _) _ _ = Nothing
