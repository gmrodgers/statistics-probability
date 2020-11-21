module Expressions
  ( Expr (..),
    eval,
    diffH,
    diff,
    integrateH,
    integrate,
    integrateWithin,
    simplifyTilStable,
  )
where

data Expr a
  = Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  | E (Expr a)
  | Ln (Expr a)
  | Var a
  | Val Float
  deriving (Eq, Show)

eval :: Expr String -> [(String, Float)] -> Float
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

diffH :: Expr String -> Expr String
diffH (Add x y) = Add (diffH x) (diffH y) -- Addition Rule
diffH (Sub x y) = Sub (diffH x) (diffH y) -- Subtraction Rule
diffH (Mult x y) = Add (Mult (diffH x) y) (Mult (diffH y) x) -- Product Rule
diffH (Div x y) = Div (Sub (Mult (diffH x) y) (Mult (diffH y) x)) (Pow y (Val 2)) -- Quotient Rule
diffH (Var _) = Val 1 -- Unitary Power
diffH (Val _) = Val 0 -- Constant
diffH (Pow x (Val a)) = Mult (diffH x) (Mult (Val a) (Pow x (Sub (Val a) (Val 1)))) -- Power Rule
diffH (E x) = Mult (diffH x) (E x) -- e Exponent
diffH (Ln x) = Mult (Div (Val 1) x) (diffH x)

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
simplify (Mult (Pow x (Val a)) (Pow y (Val b)))
  | x == y = Pow (simplify x) (Val (a + b)) -- x^1 * x^2 = x^3
  | otherwise = Mult (Pow (simplify x) (Val a)) (Pow (simplify y) (Val b))
simplify (Mult x (Pow y (Val a)))
  | x == y = Pow (simplify x) (Val (a + 1))
  | otherwise = Mult (simplify x) (simplify (Pow y (Val a)))
simplify (Mult (Div (Val 1) x) y) = Div (simplify y) (simplify x)
simplify (Mult y (Div (Val 1) x)) = Div (simplify y) (simplify x)
simplify (Mult x y) = Mult (simplify x) (simplify y) -- Simplify down the tree
simplify (Div (Val a) (Val b)) = Val (a / b) -- 2 / 1 = 2
simplify (Div (Val 0) _) = Val 0 -- 0 / x = 0
simplify (Div x (Val 1)) = simplify x -- x / 1 = x
simplify (Div (Pow x (Val a)) (Pow y (Val b)))
  | x == y = Pow (simplify x) (Val (a - b)) -- x^2 / x^1 = x^1
  | otherwise = Div (Pow (simplify x) (Val a)) (Pow (simplify y) (Val b))
simplify (Div x y) = if x == y then Val 1 else Div (simplify x) (simplify y)
simplify (Pow x (Val 1)) = simplify x -- x^1 = x
simplify (Pow _ (Val 0)) = Val 1 -- x^0 = 1
simplify (Pow x y) = Pow (simplify x) (simplify y) -- Simplify down the tree
simplify (E (Ln (Val 1))) = Val 1 -- e^lnx = 1
simplify (E x) = E (simplify x) -- Simplify down the tree
simplify (Ln (Val 1)) = Val 0 -- ln1 = 0
simplify (Ln (E (Val 1))) = Val 1 -- ln(e*x) = 1
simplify (Ln x) = Ln (simplify x) -- Simplify down the tree
simplify x = x -- if none of the above, then return

simplifyTilStable :: Expr -> Expr
simplifyTilStable x =
  let (expr : exprs) = iterate simplify x
   in (fst . head . dropWhile (uncurry (/=))) $ zip (expr : exprs) exprs

diff :: Expr String -> Expr String
diff = simplifyTilStable . diffH -- Simplify twice incase a simplify reveals previously uncaught simplifies (arbitrary)

integrateH :: Expr String -> String -> Expr String
integrateH (Add x y) wrt = Add (integrateH x wrt) (integrateH y wrt) -- Integrate down the tree
integrateH (Sub x y) wrt = Sub (integrateH x wrt) (integrateH y wrt) -- Integrate down the tree
integrateH (Mult (Val a) x) wrt = Mult (Val a) (integrateH x wrt)
integrateH (Mult x (Val a)) wrt = Mult (integrateH x wrt) (Val a)
integrateH (Pow (Var x) (Val a)) _ = Div (Pow (Var x) (Add (Val a) (Val 1))) (Add (Val a) (Val 1)) -- normal integration rule
integrateH (Pow (Val a) (Var x)) _ = Div (Pow (Val a) (Var x)) (Ln (Val a)) -- 2^x -> 2^x / ln2
integrateH (E (Val a)) wrt = Mult (E (Val a)) (Var wrt) -- e*2 -> x * e^2
integrateH (E x) _ = Div (E x) (diffH x) -- e^f(x) -> e^f(x) / f'(x)
integrateH (Ln (Val a)) wrt = Mult (Ln (Val a)) (Var wrt) -- ln2 -> x * ln2
integrateH (Var x) wrt = integrateH (Pow (Var x) (Val 1)) wrt -- x -> x^1 then integrate
integrateH (Val a) wrt = Mult (Val a) (Var wrt) -- add variable we're integrated wrt

integrate :: Expr String -> String -> Expr String
integrate x wrt = Add (simplifyTilStable $ integrateH x wrt) (Var "C")

integrateWithin :: Expr String -> String -> Float -> Float -> Float
integrateWithin x wrt a b = eval integrated [(wrt, b)] - eval integrated [(wrt, a)]
  where
    integrated = simplify . simplify $ integrateH x wrt
