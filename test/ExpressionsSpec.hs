module ExpressionsSpec
  ( main,
  )
where

import Expressions
  ( Expr (Add, Div, E, Ln, Mult, Pow, Sub, Val, Var),
    diffH,
    eval,
    integrate,
    integrateH,
    simplifyTilStable,
  )
import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)
import Test.QuickCheck

main :: IO ()
main = (hspec . parallel) $ do
  evalSpec
  diffSpec
  integrateSpec
  integrateWithConstantSpec

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
  let diffS = simplifyTilStable . diffH
   in describe "diff" $ do
        describe "Val" $
          it "goes to 0" $
            diffS (Val 1) `shouldBe` Val 0
        describe "Var" $
          it "removes the variable 'x'" $ do
            diffS (Var "x") `shouldBe` Val 1
        describe "Pow" $
          it "follows the power rule: d/dx x^a = a . x^(a-1)" $ do
            diffS (Pow (Var "x") (Val 2)) `shouldBe` Mult (Val 2) (Var "x")
        describe "Add" $
          it "differentiates each operand" $
            diffS (Add (Var "x") (Var "x")) `shouldBe` Val 2
        describe "Sub" $
          it "differentiates each operand" $
            diffS (Sub (Var "x") (Var "x")) `shouldBe` Val 0
        describe "Mult" $
          it "follows the product rule: d/dx f(x)h(x) = f'(x)h(x) + f(x)h'(x)" $ do
            diffS (Mult (Val 2) (Var "x")) `shouldBe` Val 2
            diffS (Mult (Var "x") (Ln (Var "x"))) `shouldBe` Add (Ln (Var "x")) (Val 1)
        describe "Div" $
          it "follows the quotient rule: d/dx f(x)/h(x) = [h(x)f'(x) - f(x)h'(x)] / h(x)^2" $
            let numerator = Sub (Ln (Var "x")) (Val 1)
                denominator = Pow (Ln (Var "x")) (Val 2)
             in diffS (Div (Var "x") (Ln (Var "x"))) `shouldBe` Div numerator denominator
        describe "E" $
          it "multiples E by the differential of the exponent" $ do
            diffS (E (Val 2)) `shouldBe` Val 0
            diffS (E (Var "x")) `shouldBe` E (Var "x")
            diffS (E (Mult (Val 2) (Var "x"))) `shouldBe` Mult (Val 2) (E (Mult (Val 2) (Var "x")))
            diffS (E (Pow (Var "x") (Val 2))) `shouldBe` Mult (Mult (Val 2) (Var "x")) (E (Pow (Var "x") (Val 2)))
        describe "Ln" $
          it "divides the differential of the operand by the operand" $ do
            diffS (Ln (Var "x")) `shouldBe` Div (Val 1) (Var "x")
        describe "Chain Rule" $
          it "applies chain rule: d/dx f(g(x)) = f'(g(x)) * g'(x)" $
            diffS (Ln (Pow (Var "x") (Val 2))) `shouldBe` Div (Mult (Val 2) (Var "x")) (Pow (Var "x") (Val 2))

integrateSpec :: Spec
integrateSpec =
  describe "integrateH" $ do
    describe "Val" $
      it "multiplies by variable wrt" $ do
        integrateH (Val 1) "x" `shouldBe` Mult (Val 1) (Var "x")
        integrateH (Val (-1)) "x" `shouldBe` Mult (Val (-1)) (Var "x")
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

instance Arbitrary Expr where
  arbitrary =
    sized (arbitraryExpression 3)

compoundExpr i = do
  n <- choose (0, 10)
  m <- choose (0, 10)
  x <- arbitraryExpression i n
  y <- arbitraryExpression i m
  return (x, y)

valOrVar = do
  n <- choose (0, 1)
  arbitraryExpression 3 n

arbitraryExpression :: Int -> Int -> Gen Expr
arbitraryExpression _ 0 = Val <$> arbitrary
arbitraryExpression _ 1 = do return (Var "x")
arbitraryExpression 0 _ = valOrVar
arbitraryExpression i 2 = do
  (x, y) <- compoundExpr (i -1)
  return (Add x y)
arbitraryExpression i 3 = do
  (x, y) <- compoundExpr (i -1)
  return (Sub x y)
arbitraryExpression i 4 = do
  (x, y) <- compoundExpr (i -1)
  return (Mult x y)
arbitraryExpression i 5 = do
  (x, y) <- compoundExpr (i -1)
  return (Div x y)
arbitraryExpression i 6 = do
  (x, y) <- compoundExpr (i -1)
  return (Pow x y)
arbitraryExpression i 7 = do
  (x, _) <- compoundExpr (i -1)
  return (E x)
arbitraryExpression i 8 = do
  (x, _) <- compoundExpr (i -1)
  return (Ln x)
arbitraryExpression i n = arbitraryExpression i (mod n 9)

hasC :: Expr -> Bool
hasC (Add _ (Var c)) = c == "C"
hasC _ = False

prop_hasC :: Expr -> Bool
prop_hasC x = hasC $ integrate x "x"

integrateWithConstantSpec :: Spec
integrateWithConstantSpec =
  describe "integrate" $
    it "always adds constant 'C'" $
      quickCheck prop_hasC