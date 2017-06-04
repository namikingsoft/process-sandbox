{-# LANGUAGE CPP #-}
module SandboxSpec where

import Control.Monad
import Test.Hspec
import Utility.Base
  ( (|>)
  )

type Number = Int

plus :: Number -> Number -> Number
plus x y = x + y

minus :: Number -> Number -> Number
minus x y = x - y

plusM :: Number -> Number -> Maybe Number
plusM x y = Just(x + y)

validate1 :: Number -> Maybe Number
validate1 x
  | x `mod` 2 == 0 = Just x
  | otherwise = Nothing

example0 :: Maybe Bool
example0 = Just True

example1 :: [Int]
example1 = concat $
  [[1]]
  ++
  [[2], [3]]

example2 :: Int -> Maybe String
example2 x = show <$> (+3) <$> (*3) <$> Just x

example3 :: Maybe [Int]
example3 = map (+3) <$> Just [1, 2]

spec :: Spec
spec = do

  describe "pipe" $ do
    it "should return true" $ do
      let f1 = plus 3 |> (`minus` 1)
      f1 4 `shouldBe` 6
      (validate1 $ f1 4) `shouldBe` Just 6
      let f2 = plus 3 |> (`minus` 1) |> validate1
      f2 4 `shouldBe` Just 6
      let f3 = \x -> Just x >>= \x -> Just(x + 2) >>= validate1 >>= validate1
      f3 6 `shouldBe` Just 8
      let v1 = Just 2 >>= validate1 >>= validate1
      v1 `shouldBe` Just 2
      let f4 = \x -> (plus 3 |> (`minus` 1) |> validate1) x >>= validate1
      f4 4 `shouldBe` Just 6
      let f5 = plus 3 |> (`minus` 1) |> validate1 >=> plusM 2 >=> validate1
      f5 4 `shouldBe` Just 8
      let f6 = plus 3 |> (`minus` 1) |> validate1 >=> plusM 3 >=> validate1
      f6 4 `shouldBe` Nothing
      let f7 = plus 3 |> (`minus` 1) |> validate1 >=> plusM 4 >=> plusM 2
      f7 4 `shouldBe` Just 12

  describe "example0" $ do
    it "should return maybe true" $ do
      example0 `shouldBe` Just True

  describe "example1" $ do
    it "should return maybe true" $ do
      example1 `shouldBe` [1, 2, 3]

  describe "example2" $ do
    it "should return maybe true" $ do
      example2 3 `shouldBe` Just "12"

  describe "example3" $ do
    it "should return maybe true" $ do
      example3 `shouldBe` Just [4, 5]
