module Expressions
  ( Expr
      ( Add,
        Sub,
        Mult,
        Div,
        Pow,
        E,
        Ln,
        Var,
        Val
      ),
    eval,
    diff,
    integrateWithin,
  )
where

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | E Expr
  | Ln Expr
  | Var String
  | Val Float
  deriving (Eq, Show)

eval :: Expr -> [(String, Float)] -> Float
eval (Add x y) vals = eval x vals + eval y vals
eval (Sub x y) vals = eval x vals - eval y vals
eval (Mult x y) vals = eval x vals * eval y vals
eval (Div x y) vals = eval x vals / eval y vals
eval (Pow x y) vals = eval x vals ** eval y vals
eval (E x) vals = exp (eval x vals)
eval (Ln x) vals = log (eval x vals)
eval (Var x) vals = lookup x vals
  where
    lookup :: String -> [(String, Float)] -> Float
    lookup k = snd . head . filter ((== k) . fst)
eval (Val a) _ = a

diff' :: Expr -> Expr
diff' (Add x y) = Add (diff' x) (diff' y)
diff' (Sub x y) = Sub (diff' x) (diff' y)
diff' (Mult x y) =
  Add (Mult (diff' x) y) (Mult (diff' y) x)
diff' (Div x y) =
  Div
    ( Sub
        (Mult (diff' x) y)
        (Mult (diff' y) x)
    )
    (Pow y (Val 2))
diff' (Pow x (Val a)) =
  Mult
    (Val a)
    (Pow x (Sub (Val a) (Val 1)))
diff' (E x) =
  Mult (diff' x) (E x)
diff' (Ln x) = Div (diff x) x
diff' (Var _) = Val 1
diff' (Val _) = Val 0

simplify :: Expr -> Expr
simplify (Add (Val a) (Val b)) = Val (a + b)
simplify (Add x (Val 0)) = simplify x
simplify (Add (Val 0) y) = simplify y
simplify (Add x y) = Add (simplify x) (simplify y)
simplify (Sub (Val a) (Val b)) = Val (a - b)
simplify (Sub x (Val 0)) = simplify x
simplify (Sub x y) = Sub (simplify x) (simplify y)
simplify (Mult (Val a) (Val b)) = Val (a * b)
simplify (Mult (Val 1) y) = simplify y
simplify (Mult x (Val 1)) = simplify x
simplify (Mult (Val 0) _) = Val 0
simplify (Mult _ (Val 0)) = Val 0
simplify (Mult x y) = Mult (simplify x) (simplify y)
simplify (Div (Val a) (Val b)) = Val (a / b)
simplify (Div (Val 0) y) = Val 0
simplify (Div x (Val 1)) = simplify x
simplify (Div x y) = Div (simplify x) (simplify y)
simplify (Pow x (Val 1)) = simplify x
simplify (Pow _ (Val 0)) = Val 1
simplify (Pow x y) = Pow (simplify x) (simplify y)
simplify (E (Ln (Val 1))) = Val 1
simplify (E x) = E (simplify x)
simplify (Ln (Val 1)) = Val 0
simplify (Ln (E (Val 1))) = Val 1
simplify (Ln x) = Ln (simplify x)
simplify x = x

diff :: Expr -> Expr
diff = simplify . simplify . diff'

integrate' :: Expr -> String -> Expr
integrate' (Add x y) wrt = Add (integrate' x wrt) (integrate' y wrt)
integrate' (Sub x y) wrt = Sub (integrate' x wrt) (integrate' y wrt)
integrate' (Mult x (Val a)) wrt = Mult (Val a) (integrate' x wrt)
integrate' (Mult (Val a) y) wrt = Mult (Val a) (integrate' y wrt)
integrate' (Div x (Val a)) wrt = Div (integrate' x wrt) (Val a)
integrate' (Div (Val a) y) wrt = Div (Val a) (integrate' (Div (Val 1) y) wrt)
integrate' (Pow z (Mult x y)) wrt = Add (integrate' (Pow z x) wrt) (integrate' (Pow z y) wrt)
integrate' (Pow z (Div x y)) wrt = Sub (integrate' (Pow z x) wrt) (integrate' (Pow z y) wrt)
integrate' (Pow (Var x) (Val a)) _ = Div (Pow (Var x) (Sub (Val a) (Val 1))) (Sub (Val a) (Val 1))
integrate' (Pow (Val a) (Var x)) _ = Div (Pow (Val a) (Var x)) (Ln (Val a))
integrate' (E (Val a)) wrt = Mult (E (Val a)) (Var wrt)
integrate' (E x) _ = Div (E x) (diff' x)
integrate' (Var x) wrt = integrate' (Pow (Var x) (Val 1)) wrt
integrate' (Val a) wrt = Mult (Val a) (Var wrt)

integrate :: Expr -> String -> Expr
integrate x wrt = Add (simplify . simplify $ integrate' x wrt) (Var "C")

integrateWithin :: Expr -> String -> Float -> Float -> Float
integrateWithin x wrt a b = eval integrated [(wrt, b)] - eval integrated [(wrt, a)]
  where
    integrated = simplify . simplify $ integrate' x wrt
