module Vars where

import Expr (Expr(Id))

{-|
This class allows for `x`, `y`, and `z` to be used as identifiers in arithmetic
expressions. There are two instances, one for `Double` and one for `Expr`.
-}
class Vars a where
  x, y, z :: a

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Vars Expr where
  x = Id "x"
  y = Id "y"
  z = Id "z"
