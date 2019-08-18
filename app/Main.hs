module Main where
import Data.List
import Lib

data BinOp = Conj | Disj | XOr | Impl deriving (Eq, Show)

data Logic t = Val t | Bin BinOp (Logic t) (Logic t) deriving (Eq, Show)

data CompoundRule = DoLeft CompoundRule | DoRight CompoundRule | Do Rule deriving Show

data Rule = Commutation BinOp
          | Association1 BinOp
          | Association2 BinOp          
          | Distribution1 BinOp BinOp
          | Distribution2 BinOp BinOp deriving Show

commutes :: BinOp -> Bool
commutes binOp = binOp `elem` [Conj, Disj, XOr]

associates :: BinOp -> Bool
associates binOp = binOp `elem` [Conj, Disj, XOr]

distributes :: BinOp -> BinOp -> Bool
distributes Conj XOr = True
distributes _ _ = False

applyRule :: (Eq t) => Rule -> Logic t -> Either () (Logic t)
applyRule (Commutation binOp) (Bin binOp' l r)
  | binOp == binOp' && commutes binOp = Right $ Bin binOp r l
  | otherwise                         = Left ()

applyRule (Association1 binOp) (Bin binOp' (Bin binOp'' a b) c)
  | binOp == binOp' && binOp' == binOp'' && associates binOp = Right $ Bin binOp a (Bin binOp b c)
  | otherwise       = Left ()

applyRule (Association2 binOp) (Bin binOp' a (Bin binOp'' b c))
  | binOp == binOp' && binOp' == binOp'' && associates binOp = Right $ Bin binOp (Bin binOp a b) c
  | otherwise       = Left ()

applyRule (Distribution1 binOp1 binOp2) (Bin binOp' a (Bin binOp'' b c))
  | binOp1 == binOp' && binOp2 == binOp'' && distributes binOp1 binOp2 = Right $ Bin binOp'' (Bin binOp' a b) (Bin binOp' a c)
  | otherwise = Left ()

applyRule (Distribution2 binOp1 binOp2) (Bin binOp'' (Bin binOp' a b) (Bin binOp''' a' c))
  | a == a' && binOp1 == binOp' && binOp2 == binOp'' && binOp' == binOp''' && distributes binOp1 binOp2 = Right $ (Bin binOp' a (Bin binOp'' b c))
  | otherwise = Left ()

applyRule _ _ = Left ()

main :: IO ()
main = someFunc
