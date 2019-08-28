module Rules where

import Logic
import Unification

data Syms = AA | BB | CC | DD deriving (Eq, Ord, Show)

vA = Val AA
vB = Val BB
vC = Val CC
vD = Val DD

type Rule' = (Logic Syms, Logic Syms)

elimXOr :: Rule'
elimXOr = ( Bin XOr vA vB
          , Bin Conj (Bin Disj vA vB) (Un Neg $ Bin Conj vA vB)
          )

commutes :: BinOp -> Rule'
commutes binOp = ( Bin binOp vA vB
                 , Bin binOp vB vA
                 )

associates :: BinOp -> Rule'
associates binOp = ( Bin binOp vA (Bin binOp vB vC)
                   , Bin binOp (Bin binOp vA vB) vC
                   )

distributes :: BinOp -> BinOp -> Rule'
distributes binOp1 binOp2 = ( Bin binOp1 vA (Bin binOp2 vB vC)
                            , Bin binOp2 (Bin binOp1 vA vB) (Bin binOp1 vA vC)
                            )

deMorgan :: BinOp -> BinOp -> Rule'
deMorgan binOp1 binOp2 = ( Bin binOp1 vA vB
                         , Un Neg $ Bin binOp2 (Un Neg vA) (Un Neg vB)
                         )

allRules = [ elimXOr
           , commutes Conj
           , commutes Disj
           , commutes XOr
           , associates Conj
           , associates Disj
           , associates XOr
           , distributes Conj Conj
           , distributes Conj Disj
           , distributes Conj XOr
           , deMorgan Conj Disj
           , deMorgan Disj Conj
           ]

-- TODO: What other rules exist?
-- Modus ponens
-- Modus tollens
-- Syllogisms
-- Dilemmas
-- Double negation
-- Transposition
-- Material implication
-- Material equivalence
-- Exportation
-- Importation
-- Tautology
-- DeMorgan's laws
-- Check Wikipedia page for propositional calculus

applyRuleForward :: (Eq t) => Rule' -> Logic t -> Maybe (Logic t)
applyRuleForward (l, r) logic = case (unify' l logic) of
  Just m  -> replace r m
  Nothing -> Nothing

applyRuleBackward :: (Eq t) => Rule' -> Logic t -> Maybe (Logic t)
applyRuleBackward (l, r) logic = applyRuleForward (r, l) logic
