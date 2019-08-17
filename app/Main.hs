module Main where

import Lib

data Logic t = Leaf t
             | Neg (Logic t)
             | XOr (Logic t) (Logic t)
             | Conj (Logic t) (Logic t)
             | Disj (Logic t) (Logic t) deriving Show

assoc :: Logic a -> Either () (Logic a)
assoc (Conj a (Conj b c)) = Right $ Conj (Conj a b) c
assoc (Conj (Conj a b) c) = Right $ Conj a (Conj b c)
assoc _                   = Left ()

decompXor :: Logic a -> Maybe (Logic a)
decompXor (XOr l r) = Just $ Conj (Disj l r) (Neg $ Conj l r)
decompXor _         = Nothing

deMorgan :: Logic a -> Maybe (Logic a)
deMorgan (Conj l r) = Just $ Neg $ Disj (Neg l) (Neg r)
deMorgan (Disj l r) = Just $ Neg $ Conj (Neg l) (Neg r)
deMorgan _          = Nothing

cancelNeg :: Logic a -> Logic a
cancelNeg (Neg (Neg a)) = a
cancelNeg a             = a

main :: IO ()
main = someFunc
