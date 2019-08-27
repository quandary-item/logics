module Logic where

data BinOp = Conj | Disj | XOr | Impl deriving (Eq, Show)
data UnOp = Neg deriving (Eq, Show)

data Logic t = Val t | Un UnOp (Logic t) | Bin BinOp (Logic t) (Logic t) deriving (Eq, Show)
