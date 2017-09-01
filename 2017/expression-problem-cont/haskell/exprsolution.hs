-- Expression problem solution in Haskell using typeclasses.
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.

data Constant = Constant Double
data BinaryPlus lhs rhs = BinaryPlus lhs rhs

class Stringify e where
    stringify :: e -> String

instance Stringify Constant where
    stringify (Constant num) = show num

instance (Stringify l, Stringify r) => Stringify (BinaryPlus l r) where
    stringify (BinaryPlus lhs rhs) = stringify lhs
                                     ++ " + "
                                     ++ stringify rhs

class Evaluate e where
    eval :: e -> Double

instance Evaluate Constant where
    eval (Constant num) = num

instance (Evaluate l, Evaluate r) => Evaluate (BinaryPlus l r) where
    eval (BinaryPlus lhs rhs) = (eval lhs) + (eval rhs)
