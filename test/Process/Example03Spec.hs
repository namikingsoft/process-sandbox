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
      show(parseFromString "let main = 3 + 2 in let a = 3 end") `shouldBe` "Right (LetIn (Var \"main\") (Add (Int 2) (Int 3)) [Let (Var \"a\") (Int 3)])"

