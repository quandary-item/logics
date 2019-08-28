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
