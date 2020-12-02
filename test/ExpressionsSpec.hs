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
      solveForSpec
      inverseOpSpec
      evalSpec
      diffSpec
      integrateSpec
      simplifySpec

inverseOpSpec :: Spec
inverseOpSpec =
  describe "inverseOp" $ do
    it "cancels out Adds" $ do
      inverseOp (Add (Var "x") (Val 1)) (Var "u") "x" `shouldBe` (Var "x", Sub (Var "u") (Val 1))
      inverseOp (Add (Val 1) (Var "x")) (Var "u") "x" `shouldBe` (Var "x", Sub (Var "u") (Val 1))
    it "cancels out Subs" $ do
      inverseOp (Sub (Var "x") (Val 1)) (Var "u") "x" `shouldBe` (Var "x", Add (Var "u") (Val 1))
      inverseOp (Sub (Val 1) (Var "x")) (Var "u") "x" `shouldBe` (Var "x", Div (Add (Var "u") (Val 1)) (Val (-1)))
    it "cancels out Mults" $ do
      inverseOp (Mult (Var "x") (Val 1)) (Var "u") "x" `shouldBe` (Var "x", Div (Var "u") (Val 1))
      inverseOp (Mult (Val 1) (Var "x")) (Var "u") "x" `shouldBe` (Var "x", Div (Var "u") (Val 1))
    it "cancels out Divs" $ do
      inverseOp (Div (Var "x") (Val 1)) (Var "u") "x" `shouldBe` (Var "x", Mult (Var "u") (Val 1))
      inverseOp (Div (Val 1) (Var "x")) (Var "u") "x" `shouldBe` (Var "x", Div (Val 1) (Var "u"))
    it "cancels out Pow" $ do
      inverseOp (Pow (Var "x") (Val 1)) (Var "u") "x" `shouldBe` (Var "x", Pow (Var "u") (Div (Val 1) (Val 1)))
      inverseOp (Pow (Val 1) (Var "x")) (Var "u") "x" `shouldBe` (Var "x", Div (Ln (Var "u")) (Ln (Val 1)))
    it "cancels out E" $ do
      inverseOp (E (Var "x")) (Var "u") "x" `shouldBe` (Var "x", Ln (Var "u"))
    it "cancels out Ln" $ do
      inverseOp (Ln (Var "x")) (Var "u") "x" `shouldBe` (Var "x", E (Var "u"))

solveForSpec :: Spec
solveForSpec =
  describe "solveFor" $
    it "solves for x" $ do
      solveFor (Add (Sub (Var "x") (Var "y")) (Var "z")) (Val 1) "x" `shouldBe` Add (Sub (Val 1) (Var "z")) (Var "y")
      solveFor (Add (Sub (Var "x") (Var "y")) (Var "z")) (Val 1) "z" `shouldBe` Sub (Val 1) (Sub (Var "x") (Var "y"))
      solveFor (Pow (Sub (Var "x") (Var "y")) (Var "z")) (Val 1) "z" `shouldBe` Div (Ln (Val 1)) (Ln (Sub (Var "x") (Var "y")))

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
    describe "Div" $ do
      it "follows the product + chain rule" $
        let lhs = Pow (Ln (Var "x")) (Val (-1))
            rhs = Mult (Val (-1)) (Pow (Ln (Var "x")) (Val (-2)))
         in diff (Div (Var "x") (Ln (Var "x"))) `shouldBe` Add lhs rhs
      it "gets the same value as the quotient rule" $
        let quotientRule (Div x y) = Div (Sub (Mult y (diff x)) (Mult x (diff y))) (Pow y (Val 2))
            divExpressions = Div <$> unlikelyToNaNExpressions <*> unlikelyToNaNExpressions

            error = 0.01
            prop_matchesQuotientRuleWithinError n expr =
              (&&)
                (diffRes <= (quotientRes + error))
                (diffRes >= (quotientRes - error))
              where
                diffRes = eval (diff expr) [("x", n)]
                quotientRes = eval (quotientRule expr) [("x", n)]
         in property $ forAll divExpressions (prop_matchesQuotientRuleWithinError 1)
    describe "E" $
      it "multiples E by the differential of the exponent" $ do
        diff (E (Val 2)) `shouldBe` Val 0
        diff (E (Var "x")) `shouldBe` E (Var "x")
        diff (E (Mult (Val 2) (Var "x"))) `shouldBe` Mult (E (Mult (Val 2) (Var "x"))) (Val 2)
        diff (E (Pow (Var "x") (Val 2))) `shouldBe` Mult (E (Pow (Var "x") (Val 2))) (Mult (Val 2) (Var "x"))
    describe "Ln" $ do
      it "divides the differential of the operand by the operand" $
        diff (Ln (Var "x")) `shouldBe` Div (Val 1) (Var "x")
      it "multiples E by the differential of the logarithm" $
        diff (Ln (Mult (Val 2) (Var "x"))) `shouldBe` Div (Val 1) (Var "x")
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
      it "can returns vars" $
        integrateH (E (Var "x")) "x" `shouldBe` E (Var "x")
      it "can use substitution for complicated exponents" $
        integrateH (E (Mult (Val 2) (Var "x"))) "x" `shouldBe` Mult (Div (Val 1) (Val 2)) (E (Mult (Val 2) (Var "x")))
    describe "Ln" $ do
      it "multiples by variable wrt if no variable" $
        integrateH (Ln (Val 2)) "x" `shouldBe` Mult (Ln (Val 2)) (Var "x")
      it "can do integration by parts" $
        integrateH (Ln (Var "x")) "x"
          `shouldBe` Sub (Mult (Ln (Var "x")) (Var "x")) (Mult (Val 1) (Var "x"))
      it "can use substitution for complicated logarithms" $
        integrateH (Ln (Mult (Val 2) (Var "x"))) "x"
          `shouldBe` Sub
            (Mult (Ln (Mult (Val 2) (Var "x"))) (Var "x"))
            (Mult (Val 1) (Var "x"))

simplifySpec :: Spec
simplifySpec =
  describe "simplify constituent functions" $ do
    describe "identity" $
      it "recognises identity parameter for expr" $
        let isExprOrOperand expr x y = identity expr == x || identity expr == y || identity expr == expr
            prop_returnsSame expr@(Add x y) = isExprOrOperand expr x y
            prop_returnsSame expr@(Sub x y) = isExprOrOperand expr x y
            prop_returnsSame expr@(Mult x y) = isExprOrOperand expr x y
            prop_returnsSame expr@(Div x y) = isExprOrOperand expr x y
            prop_returnsSame expr = identity expr == expr
         in property $ forAll expressions prop_returnsSame
    describe "zero" $
      it "recognises zeroer parameter for expr" $
        let prop_returnsZero expr = zero expr == expr || zero expr == Val 0
         in property $ forAll expressions prop_returnsZero
    describe "constants" $
      let x = Val 1
          y = Val 2
       in it "recognises constants to pair up" $ do
            constants (Add x y) `shouldBe` Val 3
            constants (Sub x y) `shouldBe` Val (-1)
            constants (Mult x y) `shouldBe` Val 2
            constants (Div x y) `shouldBe` Val 0.5
            constants (Pow x y) `shouldBe` Val 1
            constants (E x) `shouldBe` Val (exp 1)
            constants (Ln x) `shouldBe` Val (log 1)
    describe "inverse" $
      it "recognises inverses to cancel" $ do
        inverse (Add (Sub (Var "x") (Val 0.0)) (Val 0.0)) `shouldBe` Var "x"
        inverse (Sub (Add (Var "x") (Val 0.0)) (Val 0.0)) `shouldBe` Var "x"
        inverse (Mult (Div (Var "x") (Var "y")) (Var "y")) `shouldBe` Var "x"
        inverse (Div (Mult (Var "x") (Var "y")) (Var "y")) `shouldBe` Var "x"
        inverse (E (Ln (Var "x"))) `shouldBe` Var "x"
        inverse (Ln (E (Var "x"))) `shouldBe` Var "x"
    describe "groupExpr" $
      let x = Pow (Var "x") (Val 2)
       in it "recognises exprs to group" $ do
            groupExpr (Add (Mult (Val 1) x) (Mult (Val 2) x)) `shouldBe` Mult (Val 3) x
            groupExpr (Add x (Mult (Val 1) x)) `shouldBe` Mult (Val 2) x
            groupExpr (Sub (Mult (Val 1) x) (Mult (Val 2) x)) `shouldBe` Mult (Val (-1)) x
            groupExpr (Sub x (Mult (Val 1) x)) `shouldBe` Mult x (Val 0)
    describe "groupBases" $
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
         in property $ forAll (oneof (expressions : inversePairExpressions)) prop_isPowMultiple
    describe "unnecessaryOneSpec" $ do
      it "removes (Val 1) from Mult/Div combos" $
        let prop_isOneMultOrDivExpr orig@(Mult (Div (Val 1) y) z) = unnecessaryOne orig == Div z y
            prop_isOneMultOrDivExpr orig@(Mult z (Div (Val 1) y)) = unnecessaryOne orig == Div z y
            prop_isOneMultOrDivExpr x = unnecessaryOne x == x
         in property $ forAll expressions prop_isOneMultOrDivExpr
      describe "varToPow" $
        it "changes any Var x to a Pow x 1" $
          let prop_isNowPow (Var x) = varToPow (Var x) == Pow (Var x) (Val 1)
              prop_isNowPow x = varToPow x == x
           in property $ forAll expressions prop_isNowPow
      describe "powToVar" $
        it "changes any Pow x 1 to Var x" $
          let prop_isNowVar (Pow (Var x) (Val 1)) = powToVar (Pow (Var x) (Val 1)) == Var x
              prop_isNowVar x = powToVar x == x
           in property $ forAll expressions prop_isNowVar
    describe "simplify" $ do
      it "combines all the consituent functions and applys to whole expr tree" $
        simplify (Sub (Mult (Var "z") (Var "z")) (Mult (Val 1) (Add (Val 1) (Val 3)))) `shouldBe` Sub (Pow (Var "z") (Val 2)) (Val 4)

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

unlikelyToNaNExpressions :: Gen (Expr String)
unlikelyToNaNExpressions =
  let smallInt =
        fmap fromIntegral (arbitrary :: Gen Int)
          `suchThat` (\a -> 0 < a && a < 10000)
          `suchThat` (\a -> abs a /= 1)
   in oneof
        [ Val <$> smallInt,
          return (Var "x"),
          Add (Var "x") <$> (Val <$> smallInt),
          Sub (Var "x") <$> (Val <$> smallInt),
          Mult (Var "x") <$> (Val <$> smallInt),
          Div (Var "x") <$> (Val <$> smallInt)
        ]

valExpressions :: Gen (Expr a)
valExpressions = oneof [return (Val 0), return (Val 1), Val <$> arbitrary]

nonZeroValExpression :: Gen (Expr a)
nonZeroValExpression = oneof [return (Val 1), Val <$> arbitrary `suchThat` (> 0)]

smallIntValExpression :: Gen (Expr a)
smallIntValExpression = Val <$> fmap fromIntegral (arbitrary :: Gen Int)