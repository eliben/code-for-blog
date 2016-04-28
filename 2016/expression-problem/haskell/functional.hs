module Expressions where

data Expr = Constant Double
          | BinaryPlus Expr Expr

toString :: Expr -> String
toString (Constant c) = show c
toString (BinaryPlus lhs rhs) = toString lhs
                                ++ " + "
                                ++ toString rhs

eval :: Expr -> Double
eval (Constant c) = c
eval (BinaryPlus lhs rhs) = eval lhs + eval rhs

-- To try this, run:
-- $ ghci functional.hs
-- ... 
-- *Expressions> let x = Constant 11.1
-- *Expressions> let y = Constant 23.354
-- *Expressions> let p = BinaryPlus x y
-- *Expressions> toString p
-- "11.1 + 23.354"
-- *Expressions> eval p
-- 34.454
-- *Expressions> 
