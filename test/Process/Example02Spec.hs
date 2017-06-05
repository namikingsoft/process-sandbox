{-# LANGUAGE CPP #-}
module Process.Example02Spec where

import Test.Hspec
import Process.Example02
  ( parseFromString
  , execute
  )

spec :: Spec
spec = do

  describe "parseFromString" $ do
    it "should return true" $ do
      show(parseFromString "2 + 3") `shouldBe` "Right (Plus (Value 3) (Value 2))"

  describe "execute" $ do
    it "should return true" $ do
      execute "2 + 3 - 1" `shouldBe` Right 4

