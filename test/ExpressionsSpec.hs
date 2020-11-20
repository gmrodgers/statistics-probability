module ExpressionsSpec
  ( main,
  )
where

import Expressions (Expr (..), eval)
import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)

main :: IO ()
main = (hspec . parallel) evalSpec

evalSpec :: Spec
evalSpec =
  describe "eval" $ do
    describe "Add" $ do
      it "evaluates" $
        eval (Add (Val 2) (Val 3)) [] `shouldBe` 5.0
      it "evaluates negative" $
        eval (Add (Val 2) (Val (-3))) [] `shouldBe` (-1.0)
    describe "Sub" $ do
      it "evaluates" $
        eval (Sub (Val 2) (Val 3)) [] `shouldBe` (-1.0)
      it "evaluates negative" $
        eval (Sub (Val 2) (Val (-3))) [] `shouldBe` 5.0
    describe "Mult" $ do
      it "evaluates" $
        eval (Mult (Val 2) (Val 3)) [] `shouldBe` 6.0
      it "evaluates negative" $
        eval (Mult (Val 2) (Val (-3))) [] `shouldBe` (-6.0)
    describe "Div" $ do
      it "evaluates" $
        eval (Div (Val 6) (Val 3)) [] `shouldBe` 2.0
      it "evaluates negative" $
        eval (Div (Val 6) (Val (-3))) [] `shouldBe` (-2.0)
    describe "Pow" $ do
      it "evaluates" $
        eval (Pow (Val 2) (Val 3)) [] `shouldBe` 8.0
      it "evaluates negative" $
        eval (Pow (Val 2) (Val (-3))) [] `shouldBe` 0.125
    describe "E" $ do
      it "evaluates" $
        eval (E (Val 1)) [] `shouldBe` exp 1
    describe "Ln" $ do
      it "evaluates" $
        eval (Ln (Val 1)) [] `shouldBe` log 1
    describe "Var" $ do
      it "evaluates x" $
        eval (Var "x") [("x", 10)] `shouldBe` 10
      it "evaluates y" $
        eval (Var "y") [("x", 10), ("y", 11)] `shouldBe` 11
      it "evaluates x and y" $
        eval (Add (Var "x") (Var "y")) [("x", 10), ("y", 11)] `shouldBe` 21
    describe "Val" $ do
      it "evaluates" $
        eval (Val 10) [] `shouldBe` 10
