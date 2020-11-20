module Expressions
  ( Expr (..),
    eval,
    diffH,
    diff,
    integrate,
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

diffH :: Expr -> Expr
diffH (Add x y) = Add (diffH x) (diffH y) -- Addition Rule
diffH (Sub x y) = Sub (diffH x) (diffH y) -- Subtraction Rule
diffH (Mult x y) = Add (Mult (diffH x) y) (Mult (diffH y) x) -- Product Rule
diffH (Div x y) = Div (Sub (Mult (diffH x) y) (Mult (diffH y) x)) (Pow y (Val 2)) -- Quotient Rule
diffH (Pow x (Val a)) = Mult (Val a) (Pow x (Sub (Val a) (Val 1))) -- Power Rule
diffH (E x) = Mult (diffH x) (E x) -- e Exponent
diffH (Ln (Var x)) = Div (Val 1) (Var x) -- Natural Log
diffH (Var _) = Val 1 -- Unitary Power
diffH (Val _) = Val 0 -- Constant
-- chain rule?

simplify :: Expr -> Expr
simplify (Add (Val a) (Val b)) = Val (a + b) -- 1 + 1 = 2
simplify (Add x (Val 0)) = simplify x -- x + 0 = x
simplify (Add (Val 0) y) = simplify y -- 0 + y = y
simplify (Add x y) = Add (simplify x) (simplify y) -- Simplify down the tree
simplify (Sub (Val a) (Val b)) = Val (a - b) -- 2 - 1 = 0
simplify (Sub x (Val 0)) = simplify x -- x - 0 = x
simplify (Sub x y) = Sub (simplify x) (simplify y) -- Simplify down the tree
simplify (Mult (Val a) (Val b)) = Val (a * b) -- 1 * 2 = 2
simplify (Mult (Val 1) y) = simplify y -- 1 * y = y
simplify (Mult x (Val 1)) = simplify x -- x * 1 = x
simplify (Mult (Val 0) _) = Val 0 -- 0 * x = 0
simplify (Mult _ (Val 0)) = Val 0 -- x * 0 = 0
simplify (Mult (Pow x (Val a)) (Pow y (Val b))) = if x == y then Pow (simplify x) (Val (a + b)) else Mult (Pow (simplify x) (Val a)) (Pow (simplify y) (Val b)) -- x^1 * x^2 = x^3
simplify (Mult x y) = Mult (simplify x) (simplify y) -- Simplify down the tree
simplify (Div (Val a) (Val b)) = Val (a / b) -- 2 / 1 = 2
simplify (Div (Val 0) _) = Val 0 -- 0 / x = 0
simplify (Div x (Val 1)) = simplify x -- x / 1 = x
simplify (Div (Pow x (Val a)) (Pow y (Val b))) = if x == y then Pow (simplify x) (Val (a - b)) else Div (Pow (simplify x) (Val a)) (Pow (simplify y) (Val b)) -- x^2 / x^1 = x^1
simplify (Div x y) = Div (simplify x) (simplify y) -- Simplify down the tree
simplify (Pow x (Val 1)) = simplify x -- x^1 = x
simplify (Pow _ (Val 0)) = Val 1 -- x^0 = 1
simplify (Pow x y) = Pow (simplify x) (simplify y) -- Simplify down the tree
simplify (E (Ln (Val 1))) = Val 1 -- e^lnx = 1
simplify (E x) = E (simplify x) -- Simplify down the tree
simplify (Ln (Val 1)) = Val 0 -- ln1 = 0
simplify (Ln (E (Val 1))) = Val 1 -- ln(e*x) = 1
simplify (Ln x) = Ln (simplify x) -- Simplify down the tree
simplify x = x -- if none of the above, then return

diff :: Expr -> Expr
diff = simplify . simplify . diffH -- Simplify twice incase a simplify reveals previously uncaught simplifies (arbitrary)

integrate' :: Expr -> String -> Expr
integrate' (Add x y) wrt = Add (integrate' x wrt) (integrate' y wrt) -- Integrate down the tree
integrate' (Sub x y) wrt = Sub (integrate' x wrt) (integrate' y wrt) -- Integrate down the tree
integrate' (Mult x (Val a)) wrt = Mult (Val a) (integrate' x wrt) -- ignore constant
integrate' (Mult (Val a) y) wrt = Mult (Val a) (integrate' y wrt) -- ignore constant, should there be an Integration by parts?
integrate' (Div x (Val a)) wrt = Div (integrate' x wrt) (Val a) -- ignore constant
integrate' (Div (Val a) y) wrt = Div (Val a) (integrate' (Div (Val 1) y) wrt) -- ignore constant
integrate' (Pow z (Add x y)) wrt = Mult (integrate' (Pow z x) wrt) (integrate' (Pow z y) wrt) -- I(e^(x+y)) = I(e^x) * I(e^y)
integrate' (Pow z (Sub x y)) wrt = Div (integrate' (Pow z x) wrt) (integrate' (Pow z y) wrt) -- I(e^(x-y)) = I(e^x) / I(e^y)
integrate' (Pow (Var x) (Val a)) _ = Div (Pow (Var x) (Add (Val a) (Val 1))) (Add (Val a) (Val 1)) -- normal integration rule
integrate' (Pow (Val a) (Var x)) _ = Div (Pow (Val a) (Var x)) (Ln (Val a)) -- 2^x -> 2^x / ln2
integrate' (E (Val a)) wrt = Mult (E (Val a)) (Var wrt) -- e*2 -> x * e^2
integrate' (E x) _ = Div (E x) (diffH x) -- e^f(x) -> e^f(x) / f'(x)
integrate' (Var x) wrt = integrate' (Pow (Var x) (Val 1)) wrt -- x -> x^1 then integrate
integrate' (Val a) wrt = Mult (Val a) (Var wrt) -- add variable we're integrated wrt

integrate :: Expr -> String -> Expr
integrate x wrt = Add (simplify . simplify $ integrate' x wrt) (Var "C")

integrateWithin :: Expr -> String -> Float -> Float -> Float
integrateWithin x wrt a b = eval integrated [(wrt, b)] - eval integrated [(wrt, a)]
  where
    integrated = simplify . simplify $ integrate' x wrt
