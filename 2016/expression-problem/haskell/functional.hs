-- Sample Haskell code for a functional approach to the expression problem.
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.

module Expressions where

data Expr = Constant Double
          | BinaryPlus Expr Expr

stringify :: Expr -> String
stringify (Constant c) = show c
stringify (BinaryPlus lhs rhs) = stringify lhs
                                ++ " + "
                                ++ stringify rhs

evaluate :: Expr -> Double
evaluate (Constant c) = c
evaluate (BinaryPlus lhs rhs) = evaluate lhs + evaluate rhs

-- To try this, run:
-- $ ghci functional.hs
-- ... 
-- *Expressions> let x = Constant 1.1
-- *Expressions> let y = Constant 2.2
-- *Expressions> let p1 = BinaryPlus x y
-- *Expressions> let p2 = BinaryPlus p1 y
-- *Expressions> stringify p2
-- "1.1 + 2.2 + 2.2"
-- *Expressions> evaluate p2
-- 5.5