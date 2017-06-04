{-# LANGUAGE CPP #-}
module Process.ArithmeticSpec where

import Test.Hspec
import Process.Arithmetic
  ( execute
  )

spec :: Spec
spec = do

  describe "execute" $ do
    it "should return true" $ do
      execute "2 + 3" `shouldBe` Right 5
