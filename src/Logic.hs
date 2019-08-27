module Logic where

import qualified Data.Map as Map

data BinOp = Conj | Disj | XOr | Impl deriving (Eq, Show)
data UnOp = Neg deriving (Eq, Show)

data Logic t = Val t | Un UnOp (Logic t) | Bin BinOp (Logic t) (Logic t) deriving (Eq, Show)


replace :: (Ord t) => Logic t -> Map.Map t (Logic t) -> Logic t
replace (Val a) assignment = case (Map.lookup a assignment) of
  Just p  -> p
  Nothing -> Val a

replace (Un unOp l) assignment = Un unOp l'
  where
    l' = replace l assignment

replace (Bin unOp l r) assignment = Bin unOp l' r'
  where
    l' = replace l assignment
    r' = replace r assignment
