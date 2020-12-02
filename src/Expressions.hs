module Expressions
  ( Expr (..),
    inverseOp,
    solveFor,
    eval,
    diffH,
    diff,
    integrateH,
    integrate,
    integrateWithin,
    simplify,
    powToVar,
    inverse,
    groupExpr,
    groupBases,
    unnecessaryOne,
    constants,
    zero,
    identity,
    varToPow,
    string,
  )
where

import Data.Bifunctor

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

commute :: Expr a -> Expr a
commute (Add x y) = Add y x
commute (Mult x y) = Mult y x

-- works on leaves
emap :: (Expr a -> Expr b) -> Expr a -> Expr b
emap f (Add x y) = Add (emap f x) (emap f y)
emap f (Sub x y) = Sub (emap f x) (emap f y)
emap f (Mult x y) = Mult (emap f x) (emap f y)
emap f (Div x y) = Div (emap f x) (emap f y)
emap f (Pow x y) = Pow (emap f x) (emap f y)
emap f (E x) = E (emap f x)
emap f (Ln x) = Ln (emap f x)
emap f (Var x) = f (Var x)
emap f (Val a) = f (Val a)

-- works on nodes
apply :: (Expr String -> Expr String) -> Expr String -> Expr String
apply _ (Val a) = Val a
apply _ (Var l) = Var l
apply f (Add x y) = f (Add (apply f x) (apply f y))
apply f (Sub x y) = f (Sub (apply f x) (apply f y))
apply f (Mult x y) = f (Mult (apply f x) (apply f y))
apply f (Div x y) = f (Div (apply f x) (apply f y))
apply f (Pow x y) = f (Pow (apply f x) (apply f y))
apply f (E x) = f (E (apply f x))
apply f (Ln x) = f (Ln (apply f x))

inBrackets :: String -> String
inBrackets x = "(" ++ x ++ ")"

string :: Expr String -> String
string (Add x y) = inBrackets $ string x ++ "+" ++ string y
string (Sub x y) = inBrackets $ string x ++ "-" ++ string y
string (Mult x y) = inBrackets $ string x ++ "*" ++ string y
string (Div x y) = inBrackets $ string x ++ "/" ++ string y
string (Pow x y) = inBrackets $ string x ++ "^" ++ string y
string (E x) = inBrackets $ "e^" ++ string x
string (Ln x) = inBrackets $ "ln" ++ string x
string (Val a) = show a
string (Var l) = l

foldExpr :: Expr a -> (Expr a -> b -> b) -> b -> b
foldExpr x f acc = case x of
  Add y1 y2 -> foldExpr y1 f (foldExpr y2 f acc)
  Sub y1 y2 -> foldExpr y1 f (foldExpr y2 f acc)
  Mult y1 y2 -> foldExpr y1 f (foldExpr y2 f acc)
  Div y1 y2 -> foldExpr y1 f (foldExpr y2 f acc)
  Pow y1 y2 -> foldExpr y1 f (foldExpr y2 f acc)
  E y1 -> foldExpr y1 f acc
  Ln y1 -> foldExpr y1 f acc
  y1 -> f y1 acc

containsVar :: Expr String -> String -> Bool
containsVar x l = foldExpr (emap (isVarL l) x) anyExpr False
  where
    isVarL l (Var k) = Var (l == k)
    isVarL _ (Val _) = Var False

    anyExpr (Var b) acc = b || acc
    anyExpr _ acc = acc

inverseOp :: Expr String -> Expr String -> String -> (Expr String, Expr String)
inverseOp (Add x y) base wrt
  | containsVar x wrt && containsVar y wrt = error "i cant handle a wrt variable in both branches"
  | not (containsVar x wrt || containsVar y wrt) = error "i cant handle no wrt variables in either branch"
  | containsVar x wrt = (x, Sub base y)
  | containsVar y wrt = (y, Sub base x)
inverseOp (Sub x y) base wrt
  | containsVar x wrt && containsVar y wrt = error "i cant handle a wrt variable in both branches"
  | not (containsVar x wrt || containsVar y wrt) = error "i cant handle no wrt variables in either branch"
  | containsVar x wrt = (x, Add base y)
  | containsVar y wrt = (y, Div (Add base x) (Val (-1)))
inverseOp (Mult x y) base wrt
  | containsVar x wrt && containsVar y wrt = error "i cant handle a wrt variable in both branches"
  | not (containsVar x wrt || containsVar y wrt) = error "i cant handle no wrt variables in either branch"
  | containsVar x wrt = (x, Div base y)
  | containsVar y wrt = (y, Div base x)
inverseOp (Div x y) base wrt
  | containsVar x wrt && containsVar y wrt = error "i cant handle a wrt variable in both branches"
  | not (containsVar x wrt || containsVar y wrt) = error "i cant handle no wrt variables in either branch"
  | containsVar x wrt = (x, Mult base y)
  | containsVar y wrt = (y, Div x base)
inverseOp (Pow x y) base wrt
  | containsVar x wrt && containsVar y wrt = error "i cant handle a wrt variable in both branches"
  | not (containsVar x wrt || containsVar y wrt) = error "i cant handle no wrt variables in either branch"
  | containsVar x wrt = (x, Pow base (Div (Val 1) y))
  | containsVar y wrt = (y, Div (Ln base) (Ln x))
inverseOp (E x) base wrt
  | not (containsVar x wrt) = error "i cant handle no wrt variables in either branch"
  | containsVar x wrt = (x, Ln base)
inverseOp (Ln x) base wrt
  | not (containsVar x wrt) = error "i cant handle no wrt variables in either branch"
  | containsVar x wrt = (x, E base)

solveFor x y v = solveFor' v (x, y)
  where
    solveFor' :: String -> (Expr String, Expr String) -> Expr String
    solveFor' v (Var l, y) = if v == l then y else error "should not be here"
    solveFor' v (x, y) = solveFor' v (inverseOp x y v)

performArithmetic :: Expr String -> Float
performArithmetic (Add x y) = performArithmetic x + performArithmetic y
performArithmetic (Sub x y) = performArithmetic x - performArithmetic y
performArithmetic (Mult x y) = performArithmetic x * performArithmetic y
performArithmetic (Div x y) = performArithmetic x / performArithmetic y
performArithmetic (Pow x y) = performArithmetic x ** performArithmetic y
performArithmetic (E x) = exp (performArithmetic x)
performArithmetic (Ln x) = log (performArithmetic x)
performArithmetic (Val a) = a
performArithmetic (Var _) = error "should not be performed on Expr with Vars"

substitute :: (String, Expr String) -> Expr String -> Expr String
substitute = emap . varSub
  where
    varSub (k, x) (Var l) = if l == k then x else Var l
    varSub _ x = x

eval :: Expr String -> [(String, Float)] -> Float
eval x varSubs = performArithmetic $ foldl (flip substitute) x (map (Data.Bifunctor.second Val) varSubs)

diffH :: Expr String -> Expr String
diffH (Add x y) = Add (diffH x) (diffH y) -- Addition Rule
diffH (Sub x y) = Sub (diffH x) (diffH y) -- Subtraction Rule
diffH (Mult x y) = Add (Mult (diffH x) y) (Mult (diffH y) x) -- Product Rule
diffH (Div x y) = Div (Sub (Mult (diffH x) y) (Mult (diffH y) x)) (Pow y (Val 2)) -- Quotient Rule
diffH (Var _) = Val 1 -- Unitary Power
diffH (Val _) = Val 0 -- Constant
diffH (Pow (Var l) (Val a)) = Mult (Val a) (Pow (Var l) (Sub (Val a) (Val 1))) -- Power Rule
diffH (Pow x (Val a)) = chainRule (`Pow` Val a) x
diffH (E (Var l)) = E (Var l)
diffH (E x) = chainRule E x
diffH (Ln (Var l)) = Div (Val 1) (Var l)
diffH (Ln x) = chainRule Ln x

chainRule :: (Expr String -> Expr String) -> Expr String -> Expr String
chainRule op x =
  let u = Var "u"
      du = diffH (op u)
      dx = diffH x
   in substitute ("u", x) (Mult du dx)

identity :: Expr String -> Expr String
identity (Add x (Val 0)) = x
identity orig@(Add (Val 0) _) = (identity . commute) orig
identity (Sub x (Val 0)) = x
identity (Mult x (Val 1)) = x
identity orig@(Mult (Val 1) _) = (identity . commute) orig
identity (Div x (Val 1)) = x
identity x = x

zero :: Expr String -> Expr String
-- zero orig@(Add (Val a) (Val b))
--   | a == (- b) = Val 0
--   | otherwise = orig --- Should we replace -1.0 with Negate 1.0 so we dont have to use vals??
zero (Sub x y) = if x == y then Val 0 else Sub x y
zero (Mult _ (Val 0)) = Val 0
zero orig@(Mult (Val 0) _) = (zero . commute) orig
zero (Div (Val 0) _) = Val 0
zero (Ln (Val 1)) = Val 0
zero x = x

constants :: Expr String -> Expr String
constants (Add (Val a) (Val b)) = Val (a + b)
constants (Sub (Val a) (Val b)) = Val (a - b)
constants (Mult (Val a) (Val b)) = Val (a * b)
constants (Div (Val a) (Val b)) = Val (a / b)
constants (Pow (Val a) (Val b)) = Val (a ** b)
constants (E (Val a)) = Val (exp a)
constants (Ln (Val a)) = Val (log a)
constants x = x

inverse :: Expr String -> Expr String
inverse (Add (Sub x y) z) = if y == z then x else Add (Sub x y) z
inverse orig@(Add _ (Sub _ _)) = (inverse . commute) orig
inverse (Sub (Add x y) z)
  | x == z = y
  | y == z = x
  | otherwise = Sub (Add x y) z
inverse orig@(Sub (Val a) (Add (Val b) (Val c))) -- same negate issue as above so only vals here
  | (- b) == a = Val (- c)
  | (- c) == a = Val (- b)
  | otherwise = orig
inverse (Mult (Div x y) z) = if y == z then x else Mult (Div x y) z
inverse orig@(Mult _ (Div _ _)) = (inverse . commute) orig
inverse (Div (Mult x y) z)
  | x == z = y
  | y == z = x
  | otherwise = Div (Mult x y) z
inverse orig@(Div z (Mult x y)) -- same negate issue as above so only vals here
  | x == z = Div (Val 1) y
  | y == z = Div (Val 1) x
  | otherwise = orig
inverse (Div x y)
  | x == y = Val 1
  | otherwise = Div x y
inverse (E (Ln x)) = x
inverse (Ln (E x)) = x
inverse x = x

groupBases :: Expr String -> Expr String
groupBases orig@(Mult (Pow x z1) (Pow y z2))
  | x == y = Pow x (Add z1 z2)
  | otherwise = orig
groupBases orig@(Mult x (Mult a (Pow y z)))
  | x == y = Mult a (Pow x (Add z (Val 1)))
  | otherwise = orig
groupBases orig@(Mult x (Mult a y))
  | x == y = Mult a (Pow x (Val 2))
  | otherwise = orig
groupBases orig@(Mult (Mult _ _) _) = (groupBases . commute) orig
groupBases orig@(Div (Pow x z1) (Pow y z2))
  | x == y = Pow x (Sub z1 z2)
  | otherwise = orig
groupBases orig@(Div x (Mult a (Pow y z)))
  | x == y = Mult a (Pow x (Sub (Val 1) z))
  | otherwise = orig
groupBases orig@(Div (Mult a (Pow y z)) x)
  | x == y = Mult a (Pow x (Sub z (Val 1)))
  | otherwise = orig
groupBases x = x

groupExpr :: Expr String -> Expr String
groupExpr orig@(Add (Mult (Val a) x) (Mult (Val b) y)) =
  if x == y then Mult (Val (a + b)) x else orig
groupExpr orig@(Add x (Mult (Val a) y)) =
  if x == y then Mult (Val (a + 1)) x else orig
groupExpr orig@(Add (Mult _ _) _) = (groupExpr . commute) orig
groupExpr orig@(Add x y) =
  if x == y then Mult (Val 2) x else orig
groupExpr orig@(Sub (Mult (Val a) x) (Mult (Val b) y)) =
  if x == y then Mult (Val (a - b)) x else orig
groupExpr orig@(Sub x (Mult (Val a) y)) =
  if x == y then Mult x (Val (1 - a)) else orig
groupExpr orig@(Sub (Mult (Val a) y) x) =
  if x == y then Mult x (Val (a - 1)) else orig
groupExpr x = x

varToPow :: Expr String -> Expr String
varToPow (Var x) = Pow (Var x) (Val 1)
varToPow x = x

powToVar :: Expr String -> Expr String
powToVar (Pow (Var x) (Val 1)) = Var x
powToVar x = x

unnecessaryOne :: Expr String -> Expr String
unnecessaryOne (Mult (Div (Val 1) x) y) = Div y x
unnecessaryOne orig@(Mult _ (Div _ _)) = (unnecessaryOne . commute) orig
unnecessaryOne x = x

simplify :: Expr String -> Expr String
simplify = apply (powToVar . inverse . groupExpr . groupBases . unnecessaryOne . constants . zero . identity) . emap varToPow

diff :: Expr String -> Expr String
diff = simplify . diffH

integrateH :: Expr String -> String -> Expr String
integrateH (Add x y) wrt = Add (integrateH x wrt) (integrateH y wrt) -- Integrate down the tree
integrateH (Sub x y) wrt = Sub (integrateH x wrt) (integrateH y wrt) -- Integrate down the tree
integrateH (Mult (Val a) x) wrt = Mult (Val a) (integrateH x wrt)
integrateH orig@(Mult _ (Val _)) wrt = integrateH (commute orig) wrt
integrateH (Mult u v) wrt = integrateByParts u v wrt
integrateH (Div x y) _ = if diff y == x then y else error "no implemented"
integrateH (Pow (Var x) (Val a)) _ = Div (Pow (Var x) (Add (Val a) (Val 1))) (Add (Val a) (Val 1)) -- normal integration rule
integrateH (Pow (Val a) (Var x)) _ = Div (Pow (Val a) (Var x)) (Ln (Val a)) -- 2^x -> 2^x / ln2
integrateH (E (Val a)) wrt = Mult (E (Val a)) (Var wrt) -- e*2 -> x * e^2
integrateH (E (Var l)) wrt = if l == wrt then E (Var l) else Mult (Var wrt) (E (Var l))
integrateH (E x) wrt = integrateBySubstitution E x wrt
integrateH (Ln (Val a)) wrt = Mult (Ln (Val a)) (Var wrt) -- ln2 -> x * ln2
integrateH (Ln x) wrt = integrateByParts (Ln x) (Val 1) wrt
integrateH (Var x) wrt = integrateH (Pow (Var x) (Val 1)) wrt -- x -> x^1 then integrate
integrateH (Val a) wrt = Mult (Val a) (Var wrt) -- add variable we're integrated wrt

integrateBySubstitution :: (Expr String -> Expr String) -> Expr String -> String -> Expr String
integrateBySubstitution f x wrt =
  let u = Var "u"
      solvedX = solveFor x u wrt
      solvedDX = solveFor (Mult (Var "dx") (diff x)) (Val 1) "dx"
   in substitute ("u", x) (Mult (substitute ("x", solvedX) solvedDX) (integrateH (f u) "u"))

integrateByParts :: Expr String -> Expr String -> String -> Expr String
integrateByParts u dv wrt =
  let du = diff u
      v = integrateH dv wrt
   in Sub (Mult u v) (integrateH (Mult du v) wrt)

integrate :: Expr String -> String -> Expr String
integrate x wrt = Add (simplify $ integrateH x wrt) (Var "C")

integrateWithin :: Expr String -> String -> Float -> Float -> Float
integrateWithin x wrt a b = eval ((simplify . integrateH x) wrt) [(wrt, b)] - eval ((simplify . integrateH x) wrt) [(wrt, a)]
