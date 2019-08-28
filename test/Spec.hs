{-# language DeriveGeneric #-}
import Test.QuickCheck
import GHC.Generics
import Generic.Random

import Logic

instance Arbitrary BinOp where
  arbitrary = genericArbitraryU

instance Arbitrary UnOp where
  arbitrary = genericArbitraryU

instance (Arbitrary t) => Arbitrary (Logic t) where
  arbitrary = genericArbitraryU

main :: IO ()
main = do
  pure

-- Test ideas:
-- Create a random rule and a random expression
-- If the rule can be applied to the expression, then ????

-- replace passes if all of the variables in the exp are provided in the mapping
-- maybe need an aux method to extract the set of variables in an exp?
-- this aux method should be tested too by itself. idk how

-- if a rule can be evaluated forwards, it can be evaluated backwards with the result. i.e. it must be able to be evaluated symmetrically
