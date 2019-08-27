module Main where
import Data.List
import Lib
import Logic

data CompoundRule = DoLeft CompoundRule | DoRight CompoundRule | Do Rule deriving Show

applyCompoundRule :: (Eq t) => CompoundRule -> Logic t -> Either () (Logic t)
applyCompoundRule (DoLeft rule) (Bin binOp l r) = do
  l' <- applyCompoundRule rule l
  pure $ Bin binOp l' r

applyCompoundRule (DoRight rule) (Bin binOp l r) = do
  r' <- applyCompoundRule rule r
  pure $ Bin binOp l r'

applyCompoundRule (Do rule) logic = applyRule rule logic


data Rule = Commutation BinOp
          | Association1 BinOp
          | Association2 BinOp          
          | Distribution1 BinOp BinOp
          | Distribution2 BinOp BinOp
          | Intro BinOp
          | Elim BinOp deriving Show

commutes :: BinOp -> Bool
commutes binOp = binOp `elem` [Conj, Disj, XOr]

associates :: BinOp -> Bool
associates binOp = binOp `elem` [Conj, Disj, XOr]

distributes :: BinOp -> BinOp -> Bool
distributes Conj Conj = True
distributes Conj Disj = True
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

applyRule (Elim XOr) (Bin binOp l r)
  | binOp == XOr = Right $ Bin Conj (Bin Disj l r) (Un Neg $ Bin Conj l r)
  | otherwise    = Left ()

applyRule (Intro XOr) (Bin Conj (Bin Disj l r) (Un Neg (Bin Conj l' r')))
  | l == l' && r == r' = Right $ Bin XOr l r
  | otherwise    = Left ()


applyRule _ _ = Left ()

doRules :: (Eq t, Show t) => (Logic t) -> [CompoundRule] -> IO ()
doRules logic [] = putStrLn $ show logic
doRules logic (rule:rules) = do
  putStrLn $ show logic
  putStrLn $ "applying " ++ show rule

  case applyCompoundRule rule logic of
    Right logic' -> doRules logic' rules
    Left () -> pure ()

main :: IO ()
main = someFunc
