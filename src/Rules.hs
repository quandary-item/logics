module Rules where

import Logic
import Unification

data Syms = AA | BB | CC | DD deriving (Eq, Ord, Show)

type Rule' = (Logic Syms, Logic Syms)

elimXOr :: Rule'
elimXOr = ( Bin XOr (Val AA) (Val BB)
          , Bin Conj (Bin Disj (Val AA) (Val AA)) (Un Neg $ Bin Conj (Val AA) (Val BB))
          )

commutes :: BinOp -> Rule'
commutes binOp = ( Bin binOp (Val AA) (Val BB)
                 , Bin binOp (Val BB) (Val AA)
                 )

associates :: BinOp -> Rule'
associates binOp = ( Bin binOp (Val AA) (Bin binOp (Val BB) (Val CC))
                   , Bin binOp (Bin binOp (Val AA) (Val BB)) (Val CC)
                   )

applyRuleForward :: (Eq t) => Rule' -> Logic t -> Maybe (Logic t)
applyRuleForward (l, r) logic = case (unify' l logic) of
  Just m  -> replace r m
  Nothing -> Nothing

applyRuleBackward :: (Eq t) => Rule' -> Logic t -> Maybe (Logic t)
applyRuleBackward (l, r) logic = applyRuleForward (r, l) logic
