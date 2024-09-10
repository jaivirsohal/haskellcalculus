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
  negate      = undefined
  (+)         = undefined
  (*)         = undefined

instance Fractional Expr where
  fromRational = undefined
  (/)          = undefined

instance Floating Expr where
  sin = undefined
  cos = undefined
  log = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp = undefined

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Expr -> Env -> Double
eval = undefined

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr = undefined

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff = undefined

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin = undefined
