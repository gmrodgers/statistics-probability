module ExpressionsSpec
  ( mainSpec,
  )
where

import Debug.Trace
import Expressions
import Test.Hspec
import Test.QuickCheck

mainSpec :: Spec
mainSpec =
  parallel $
    describe "expressions" $ do
      evalSpec
      diffSpec
      integrateSpec
      identitySpec
      zeroSpec
      constantsSpec
      inverseSpec
      groupExprSpec
      groupBasesSpec
      unnecessaryOneSpec
      varToPowSpec
      powToVarSpec

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
  describe "diff" $ do
    describe "Val" $
      it "goes to 0" $
        diff (Val 1) `shouldBe` Val 0
    describe "Var" $
      it "removes the variable 'x'" $ do
        diff (Var "x") `shouldBe` Val 1
    describe "Pow" $
      it "follows the power rule: d/dx x^a = a . x^(a-1)" $ do
        diff (Pow (Var "x") (Val 2)) `shouldBe` Mult (Val 2) (Var "x")
    describe "Add" $
      it "differentiates each operand" $
        diff (Add (Var "x") (Var "x")) `shouldBe` Val 2
    describe "Sub" $
      it "differentiates each operand" $
        diff (Sub (Var "x") (Var "x")) `shouldBe` Val 0
    describe "Mult" $
      it "follows the product rule: d/dx f(x)h(x) = f'(x)h(x) + f(x)h'(x)" $ do
        diff (Mult (Val 2) (Var "x")) `shouldBe` Val 2
        diff (Mult (Var "x") (Ln (Var "x"))) `shouldBe` Add (Ln (Var "x")) (Val 1)
    describe "Div" $
      it "follows the quotient rule: d/dx f(x)/h(x) = [h(x)f'(x) - f(x)h'(x)] / h(x)^2" $
        let numerator = Sub (Ln (Var "x")) (Val 1)
            denominator = Pow (Ln (Var "x")) (Val 2)
         in diff (Div (Var "x") (Ln (Var "x"))) `shouldBe` Div numerator denominator
    describe "E" $
      it "multiples E by the differential of the exponent" $ do
        diff (E (Val 2)) `shouldBe` Val 0
        diff (E (Var "x")) `shouldBe` E (Var "x")
        diff (E (Mult (Val 2) (Var "x"))) `shouldBe` Mult (Val 2) (E (Mult (Val 2) (Var "x")))
        diff (E (Pow (Var "x") (Val 2))) `shouldBe` Mult (E (Pow (Var "x") (Val 2))) (Mult (Val 2) (Var "x"))
    describe "Ln" $
      it "divides the differential of the operand by the operand" $ do
        diff (Ln (Var "x")) `shouldBe` Div (Val 1) (Var "x")
    describe "Chain Rule" $
      it "applies chain rule: d/dx f(g(x)) = f'(g(x)) * g'(x)" $
        diff (Ln (Pow (Var "x") (Val 2))) `shouldBe` Div (Mult (Val 2) (Var "x")) (Pow (Var "x") (Val 2))

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

identitySpec :: Spec
identitySpec =
  describe "identity" $
    it "recognises identity parameter for expr" $
      let isExprOrOperand expr x y = identity expr == x || identity expr == y || identity expr == expr

          prop_returnsSame expr@(Add x y) = isExprOrOperand expr x y
          prop_returnsSame expr@(Sub x y) = isExprOrOperand expr x y
          prop_returnsSame expr@(Mult x y) = isExprOrOperand expr x y
          prop_returnsSame expr@(Div x y) = isExprOrOperand expr x y
          prop_returnsSame expr = identity expr == expr
       in property $ forAll expressions prop_returnsSame

zeroSpec :: Spec
zeroSpec =
  describe "zero" $
    it "recognises zeroer parameter for expr" $
      let prop_returnsZero expr = zero expr == expr || zero expr == Val 0
       in property $ forAll expressions prop_returnsZero

constantsSpec :: Spec
constantsSpec =
  describe "constants" $ do
    it "recognises constants to pair up" $
      let prop_returnsFunctionOF expr@(Add (Val x) (Val y)) = constants expr == Val (x + y)
          prop_returnsFunctionOF expr@(Sub (Val x) (Val y)) = constants expr == Val (x - y)
          prop_returnsFunctionOF expr@(Mult (Val x) (Val y)) = constants expr == Val (x * y)
          prop_returnsFunctionOF expr@(Div (Val x) (Val y)) = constants expr == Val (x / y)
          prop_returnsFunctionOF expr@(Pow (Val x) (Val y)) = constants expr == Val (x ** y)
          prop_returnsFunctionOF expr@(E (Val y)) = constants expr == Val (exp y)
          prop_returnsFunctionOF expr@(Ln (Val y)) = constants expr == Val (log y)
          prop_returnsFunctionOF x = constants x == x
       in property $ forAll expressions prop_returnsFunctionOF

inverseSpec :: Spec
inverseSpec =
  describe "inverse" $ do
    it "recognises inverses to cancel" $
      let isExprOrInverse expr x y = inverse expr == x || inverse expr == y
          inversePairExpressions =
            let x = Val <$> arbitrary
                y = Var <$> arbitrary
                f1 x a = x <$> a
                f2 x a b = x <$> a <*> b
             in [ f2 Add x (f2 Sub y x),
                  f2 Add (f2 Sub y x) x,
                  f2 Sub (f2 Add y x) x,
                  f2 Mult (f2 Div y x) x,
                  f2 Mult x (f2 Div y x),
                  f2 Div (f2 Mult y x) x,
                  f1 Ln (f1 E x),
                  f1 E (f1 Ln x)
                ]

          prop_returnsInverseCancelledOut expr@(Add x y) = isExprOrInverse expr x y
          prop_returnsInverseCancelledOut expr@(Sub x y) = isExprOrInverse expr x y
          prop_returnsInverseCancelledOut expr@(Mult x y) = isExprOrInverse expr x y
          prop_returnsInverseCancelledOut expr@(Div x y) = isExprOrInverse expr x y
          prop_returnsInverseCancelledOut expr@(E x) = isExprOrInverse expr x x
          prop_returnsInverseCancelledOut expr@(Ln x) = isExprOrInverse expr x x
          prop_returnsInverseCancelledOut x = inverse x == x
       in verboseCheck $ forAll (oneof (expressions : inversePairExpressions)) prop_returnsInverseCancelledOut

groupExprSpec :: Spec
groupExprSpec =
  describe "groupExpr" $ do
    it "recognises exprs to group" $
      let prop_isConstantByExpr expr = case groupExpr expr of
            Mult (Val _) _ -> True
            Val 2 -> True
            Val 0 -> True
            _ -> groupExpr expr == expr
          inversePairExpressions =
            let x = Val <$> arbitrary
                y = expressions
                f2 f a b = f <$> a <*> b
             in [ f2 Add x (f2 Mult y x),
                  f2 Add (f2 Mult y x) x,
                  f2 Sub (f2 Mult y x) x,
                  f2 Sub x (f2 Mult y x)
                ]
       in verboseCheck $ forAll (oneof (expressions : inversePairExpressions)) prop_isConstantByExpr

groupBasesSpec :: Spec
groupBasesSpec =
  describe "groupBases" $ do
    it "recognises bases to group" $
      let prop_isPowMultiple expr = case groupBases expr of
            Pow _ _ -> True
            Mult _ (Pow _ _) -> True
            _ -> groupBases expr == expr
          inversePairExpressions =
            let base = expressions
                x = Pow <$> base <*> expressions
                y = Pow <$> base <*> expressions

                f2 f a b = f <$> a <*> b
             in [ f2 Mult x y,
                  f2 Div x y,
                  f2 Mult x (f2 Mult y expressions),
                  f2 Div x (f2 Mult y expressions)
                ]
       in verboseCheck $ forAll (oneof (expressions : inversePairExpressions)) prop_isPowMultiple

unnecessaryOneSpec :: Spec
unnecessaryOneSpec =
  describe "unnecessaryOneSpec" $ do
    it "removes (Val 1) from Mult/Div combos" $
      let prop_isOneMultOrDivExpr orig@(Mult (Div (Val 1) y) z) = unnecessaryOne orig == Div z y
          prop_isOneMultOrDivExpr orig@(Mult z (Div (Val 1) y)) = unnecessaryOne orig == Div z y
          prop_isOneMultOrDivExpr x = unnecessaryOne x == x
       in verboseCheck $ forAll expressions prop_isOneMultOrDivExpr

varToPowSpec :: Spec
varToPowSpec =
  describe "varToPow" $ do
    it "changes any Var x to a Pow x 1" $
      let prop_isNowPow (Var x) = varToPow (Var x) == Pow (Var x) (Val 1)
          prop_isNowPow x = varToPow x == x
       in verboseCheck $ forAll expressions prop_isNowPow

powToVarSpec :: Spec
powToVarSpec =
  describe "powToVar" $ do
    it "changes any Pow x 1 to Var x" $
      let prop_isNowVar (Pow (Var x) (Val 1)) = powToVar (Pow (Var x) (Val 1)) == Var x
          prop_isNowVar x = powToVar x == x
       in verboseCheck $ forAll expressions prop_isNowVar

-- GENERATORS FOR QUICKCHECK TESTS

expressions :: Gen (Expr String)
expressions =
  oneof
    [ Add <$> valExpressions <*> valExpressions,
      Sub <$> valExpressions <*> valExpressions,
      Mult <$> valExpressions <*> valExpressions,
      Div <$> valExpressions <*> nonZeroValExpression,
      Pow <$> valExpressions <*> smallIntValExpression,
      E <$> valExpressions,
      Ln <$> nonZeroValExpression
    ]

valExpressions :: Gen (Expr a)
valExpressions = oneof [return (Val 0), return (Val 1), Val <$> arbitrary]

nonZeroValExpression :: Gen (Expr a)
nonZeroValExpression = oneof [return (Val 1), Val <$> arbitrary `suchThat` (> 0)]

smallIntValExpression :: Gen (Expr a)
smallIntValExpression = Val <$> fmap fromIntegral (arbitrary :: Gen Int)