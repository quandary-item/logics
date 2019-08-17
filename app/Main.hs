module Main where

import Lib

data BinOp = Conj | Disj | XOr | Impl deriving (Eq, Show)

data Logic t = Val t | Bin BinOp (Logic t) (Logic t) deriving Show

class Rule r where
  applyRule :: r -> Logic t -> Either () (Logic t)


commutativeBinOps = [Conj, Disj, XOr]

data Commute = Commute BinOp
instance Rule Commute where
  applyRule (Commute binOp) (Bin binOp' l r)
    | binOp == binOp' && binOp `elem` commutativeBinOps = Right $ Bin binOp r l
    | otherwise           = Left ()
  applyRule _ _ = Left ()

instance Show Commute where
  show (Commute binOp) = show binOp ++ " is commutative"

main :: IO ()
main = someFunc
