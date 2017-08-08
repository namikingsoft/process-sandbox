{-# LANGUAGE CPP #-}
module Process.Example03Spec where

import Test.Hspec
import Process.Example03
  ( parseFromString
  )

spec :: Spec
spec = do

  describe "parseFromString" $ do
    it "should return true" $ do
      show(parseFromString "let main = x + 2 in let x = 3 end") `shouldBe` "Right (LetIn (Var \"main\") (Add (Int 2) (Var \"x\")) [Let (Var \"x\") (Int 3)])"

