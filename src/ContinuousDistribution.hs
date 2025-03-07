module ContinuousDistribution
  ( ContinuousDist (..),
    pdf,
    mean,
    variance,
    numericalCDF,
    exponentialPDF,
    uniformPDF,
    normalPDF,
    cdf,
  )
where

import Expressions (Expr (..), eval, integrateWithin, typedOp)

data ContinuousDist = Exp Float | Unif Float Float | Norm Float Float | Custom (Expr String) Float Float

exponentialPDF :: Float -> Expr String
exponentialPDF la = Mult (Val la) (E (Mult (Neg (Val la)) (Var "x")))

uniformPDF :: Float -> Float -> Expr String
uniformPDF a b = typedOp (/) 1 (b - a)

normalPDF :: Float -> Float -> Expr String
normalPDF m v = Div numerator denominator
  where
    exponentVar = Pow (Div (Sub (Var "x") (Val m)) (Val (sqrt v))) (Val 2)
    numerator = E (Mult (Neg (Val 0.5)) exponentVar)
    denominator = Val (sqrt v * sqrt (2 * pi))

pdf :: ContinuousDist -> Expr String
pdf (Exp la) = exponentialPDF la
pdf (Unif a b) = uniformPDF a b
pdf (Norm m v) = normalPDF m v
pdf (Custom x _ _) = x

numericalCDF :: ContinuousDist -> Float -> Float -> Int -> Float
numericalCDF dist a b splits = (b - a) * sum (map (\y -> eval (pdf dist) [("x", y)]) pts) / fromIntegral splits
  where
    increment = (b - a) / fromIntegral splits
    pts = take splits $ iterate (+ increment) a

cdf :: ContinuousDist -> Float -> Float -> Float
cdf dist = integrateWithin (pdf dist) "x"

expectedValue :: ContinuousDist -> (Expr String -> Expr String) -> Float -> Float -> Float
expectedValue dist tr = integrateWithin ((tr . pdf) dist) "x"

mean :: ContinuousDist -> Float
mean (Exp la) = 1 / la
mean (Unif a b) = (a + b) / 2
mean (Norm m _) = m
mean (Custom x a b) = expectedValue (Custom x a b) (Mult (Var "x")) a b

variance :: ContinuousDist -> Float
variance (Exp la) = 1 / la ^ 2
variance (Unif a b) = (b - a) ^ 2 / 12
variance (Norm _ v) = v
variance (Custom x a b) = eX2 - (eX ** 2)
  where
    eX2 = expectedValue (Custom x a b) (\x -> Pow x (Val 2)) a b
    eX = expectedValue (Custom x a b) id a b