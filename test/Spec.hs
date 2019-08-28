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
main = putStrLn "Test suite not yet implemented"
