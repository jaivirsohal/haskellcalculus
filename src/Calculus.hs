{-# LANGUAGE StandaloneDeriving #-}
module Calculus (lookUp, eval, showExpr, diff, maclaurin) where

import Vars
import Expr

import Data.Maybe

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

-- Comment this out if you want to implement your own instance in terms
-- of `showExpr`
deriving instance Show Expr

instance Num Expr where
  fromInteger = undefined
  negate      = Neg
  (+)         = Add
  (*)         = Mul

instance Fractional Expr where
  fromRational = undefined
  (/)          = Div

instance Floating Expr where
  sin = Sin
  cos = Cos
  log = Log

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key pair = fromJust (lookup key pair)

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Expr -> Env -> Double
eval (Val x) _ = x
eval (Id x) env = lookUp x env 
eval (Add x y) env = eval x env + eval y env
eval (Mul x y) env = eval x env * eval y env
eval (Div x y) env = eval x env / eval y env
eval (Neg x) env = -(eval x env)
eval (Sin x) env = sin(eval x env)
eval (Cos x) env = cos(eval x env)
eval (Log x) env = log(eval x env)

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr (Val x) = show x
showExpr (Id x) = x
showExpr (Add x y) = "(" ++ showExpr x ++ "+" ++ showExpr y ++ ")"
showExpr (Mul x y) = "(" ++ showExpr x ++ "*" ++ showExpr y ++ ")"
showExpr (Div x y) = "(" ++ showExpr x ++ "/" ++ showExpr y ++ ")"
showExpr (Neg x) = "-(" ++ showExpr x ++ ")"
showExpr (Sin x) = " (sin" ++ showExpr x ++ ")"
showExpr (Cos x) = " (cos" ++ showExpr x ++ ")"
showExpr (Log x) = " (log" ++ showExpr x ++ ")"

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Val a) x = Val 0
diff (Id a) x =  if a == x then Val 1 else Val 0
diff (Add a b) x = Add (diff a x) (diff b x)
diff (Mul a b) x = Add (Mul a (diff b x)) (Mul (diff a x) b)
diff (Div a b) x = Div (Add (Mul (diff a x) b) (Neg (Mul a (diff b x)))) (Mul b b)
diff (Neg a) x = Neg (diff a x)
diff (Sin a) x = Mul (Cos a) (diff a x)
diff (Cos a) x = Neg (Mul (Sin a) (diff a x))
diff (Log a) x = Div (diff a x) a


{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin expr x n = sum results
  where facts = genFacts n
        diffs = take n (iterate (`diff` "x") expr)
        evals = [eval d [("x", 0)] | d <- diffs]
        indexes = take n [0..]
        powers = [x^index | index <- indexes]
        numerators = zipWith (*) evals powers
        results = zipWith (/) numerators facts
        --Generates n number of factorials
        genFacts :: Int -> [Double]
        genFacts n = take n (scanl (*) 1 [1..])





