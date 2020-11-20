module ExpressionsSpec
  ( main,
  )
where

import Expressions
  ( Expr (Add, Div, E, Ln, Mult, Pow, Sub, Val, Var),
    diffH,
    eval,
    integrateH,
  )
import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)

main :: IO ()
main = (hspec . parallel) $ do
  evalSpec
  diffSpec
  integrateSpec

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
    describe "E" $
      it "evaluates" $
        eval (E (Val 1)) [] `shouldBe` exp 1
    describe "Ln" $
      it "evaluates" $
        eval (Ln (Val 1)) [] `shouldBe` log 1
    describe "Var" $ do
      it "evaluates x" $
        eval (Var "x") [("x", 10)] `shouldBe` 10
      it "evaluates y" $
        eval (Var "y") [("x", 10), ("y", 11)] `shouldBe` 11
      it "evaluates recursively" $ do
        eval (Add (Var "x") (Var "y")) [("x", 2), ("y", 3)] `shouldBe` 5
        eval (Sub (Var "x") (Var "y")) [("x", 2), ("y", 3)] `shouldBe` (-1)
        eval (Mult (Var "x") (Var "y")) [("x", 2), ("y", 3)] `shouldBe` 6
        eval (Div (Var "x") (Var "y")) [("x", 3), ("y", 2)] `shouldBe` 1.5
        eval (Pow (Var "x") (Var "y")) [("x", 3), ("y", 2)] `shouldBe` 9
        eval (E (Var "x")) [("x", 3), ("y", 2)] `shouldBe` exp 3
        eval (Ln (Var "x")) [("x", 3), ("y", 2)] `shouldBe` log 3
    describe "Val" $
      it "evaluates" $
        eval (Val 10) [] `shouldBe` 10

diffSpec :: Spec
diffSpec =
  describe "diffH" $ do
    describe "Val" $
      it "goes to 0" $
        diffH (Val 1) `shouldBe` Val 0
    describe "Var" $
      it "removes the variable 'x'" $ do
        diffH (Var "x") `shouldBe` Val 1
    describe "Pow" $
      it "follows the power rule: d/dx x^a = a . x^(a-1)" $ do
        diffH (Pow (Var "x") (Val 2)) `shouldBe` Mult (Val 2) (Pow (Var "x") (Sub (Val 2) (Val 1)))
    describe "Add" $
      it "differentiates each operand" $
        diffH (Add (Var "x") (Var "x")) `shouldBe` Add (Val 1) (Val 1)
    describe "Sub" $
      it "differentiates each operand" $
        diffH (Sub (Var "x") (Var "x")) `shouldBe` Sub (Val 1) (Val 1)
    describe "Mult" $
      it "follows the product rule: d/dx f(x)h(x) = f'(x)h(x) + f(x)h'(x)" $ do
        diffH (Mult (Val 2) (Var "x")) `shouldBe` Add (Mult (Val 0) (Var "x")) (Mult (Val 1) (Val 2))
        diffH (Mult (Var "x") (Ln (Var "x"))) `shouldBe` Add (Mult (Val 1) (Ln (Var "x"))) (Mult (Div (Val 1) (Var "x")) (Var "x"))
    describe "Div" $
      it "follows the quotient rule: d/dx f(x)/h(x) = [h(x)f'(x) - f(x)h'(x)] / h(x)^2" $
        let numerator = Sub (Mult (Val 1) (Ln (Var "x"))) (Mult (Div (Val 1) (Var "x")) (Var "x"))
            denominator = Pow (Ln (Var "x")) (Val 2)
         in diffH (Div (Var "x") (Ln (Var "x"))) `shouldBe` Div numerator denominator
    describe "E" $
      it "multiples E by the differential of the exponent" $ do
        diffH (E (Val 2)) `shouldBe` Mult (Val 0) (E (Val 2))
        diffH (E (Var "x")) `shouldBe` Mult (Val 1) (E (Var "x"))
        diffH (E (Mult (Val 2) (Var "x"))) `shouldBe` Mult (Add (Mult (Val 0) (Var "x")) (Mult (Val 1) (Val 2))) (E (Mult (Val 2) (Var "x")))
        diffH (E (Pow (Var "x") (Val 2))) `shouldBe` Mult (Mult (Val 2) (Pow (Var "x") (Sub (Val 2) (Val 1)))) (E (Pow (Var "x") (Val 2)))
    describe "Ln" $
      it "divides the differential of the operand by the operand" $ do
        diffH (Ln (Var "x")) `shouldBe` Div (Val 1) (Var "x")

integrateSpec :: Spec
integrateSpec =
  describe "integrateH" $ do
    describe "Val" $
      it "multiplies by variable wrt" $ do
        integrateH (Val 1) "x" `shouldBe` Mult (Val 1) (Var "x")
        integrateH (Val 2) "y" `shouldBe` Mult (Val 2) (Var "y")
    describe "Var" $
      it "increments the power and divides by the new power" $
        integrateH (Var "x") "x" `shouldBe` Div (Pow (Var "x") (Add (Val 1) (Val 1))) (Add (Val 1) (Val 1))
    describe "Pow" $ do
      it "a^x divides by the natural log of a" $
        integrateH (Pow (Val 2) (Var "x")) "x" `shouldBe` Div (Pow (Val 2) (Var "x")) (Ln (Val 2))
      it "x^a increments the power and divides by the new power" $ do
        integrateH (Pow (Var "x") (Val 2)) "x" `shouldBe` Div (Pow (Var "x") (Add (Val 2) (Val 1))) (Add (Val 2) (Val 1))
        integrateH (Pow (Var "x") (Val 0.5)) "x" `shouldBe` Div (Pow (Var "x") (Add (Val 0.5) (Val 1))) (Add (Val 0.5) (Val 1))
        integrateH (Pow (Var "x") (Val (-0.5))) "x" `shouldBe` Div (Pow (Var "x") (Add (Val (-0.5)) (Val 1))) (Add (Val (-0.5)) (Val 1))
    describe "Add" $
      it "integrates the individual operands" $
        integrateH (Add (Val 1) (Val 2)) "x" `shouldBe` Add (Mult (Val 1) (Var "x")) (Mult (Val 2) (Var "x"))
    describe "Sub" $
      it "integrates the individual operands" $
        integrateH (Sub (Val 1) (Val 2)) "x" `shouldBe` Sub (Mult (Val 1) (Var "x")) (Mult (Val 2) (Var "x"))
    describe "E" $ do
      it "multiples by variable wrt if no variable" $
        integrateH (E (Val 2)) "x" `shouldBe` Mult (E (Val 2)) (Var "x")
      it "divides by the constant in the exponent" $
        integrateH (E (Var "x")) "x" `shouldBe` Div (E (Var "x")) (Val 1.0)
    describe "Ln" $ do
      it "multiples by variable wrt if no variable" $
        integrateH (Ln (Val 2)) "x" `shouldBe` Mult (Ln (Val 2)) (Var "x")
