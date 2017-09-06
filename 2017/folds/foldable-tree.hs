-- Folding binary trees in Haskell.
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.
import qualified Data.Foldable as F
import Data.Monoid

-- The relevant part(s) of Foldable from its source:
--
-- class Foldable t where
--    -- | Combine the elements of a structure using a monoid.
--    fold :: Monoid m => t m -> m
--    fold = foldMap id
--
--    -- | Map each element of the structure to a monoid,
--    -- and combine the results.
--    foldMap :: Monoid m => (a -> m) -> t a -> m
--    -- This INLINE allows more list functions to fuse. See Trac #9848.
--    foldMap f = foldr (mappend . f) mempty

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
  deriving Show

-- Note: foldMap :: Monoid m => (a -> m) -> t a -> m
--
-- The types here are:
--  * a: value inside the tree
--  * t: Tree
--  * m: the (Monoid) value returned by `f` when taking a value of type `a`.

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node x left right) = F.foldMap f left <> f x <> F.foldMap f right

-- Foldable uses the Endo trick [https://en.wikibooks.org/wiki/Haskell/Foldable]
-- to have foldr out of foldMap

t1 = Node 10 (Node 20 (Leaf 4) (Leaf 6)) (Leaf 7)

-- Now can do:
-- > Data.Foldable.foldr (+) 0 t1
--
-- Plus the other methods defined by Foldable, like:
-- > Data.Foldable.foldr1 (+) t1
--
-- and toList, length, etc.

-- Manual folding implementation; a is a Monoid. So we need a function taking
-- a value and returning a monoid (for example, Sum).
foldTree :: Monoid a => (b -> a) -> Tree b -> a
foldTree _ Empty = mempty
foldTree f (Leaf x) = f x
foldTree f (Node x left right) = (foldTree f left) <> f x <> (foldTree f right)
