{-# LANGUAGE CPP #-}
module Process.Example01Spec where

import Test.Hspec
import Process.Example01
  ( execute
  )

spec :: Spec
spec = do

  describe "execute" $ do
    it "should return true" $ do
      execute "2 + 3" `shouldBe` Right 5
