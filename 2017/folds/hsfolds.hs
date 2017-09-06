-- Folds in Haskell.
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.
myproduct [] = 1
myproduct (x:xs) = x * myproduct xs

mydouble [] = []
mydouble (x:xs) = [2 * x] ++ mydouble xs

myfoldr :: (b -> a -> a) -> a -> [b] -> a
myfoldr _ z [] = z
myfoldr f z (x:xs) = f x (myfoldr f z xs)

myproductWithFoldr = myfoldr (*) 1

mydoubleWithFoldr = myfoldr (\x acc -> [2 * x] ++ acc) []

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl _ z [] = z
myfoldl f z (x:xs) = myfoldl f (f z x) xs

digitsToNumWithFoldl = myfoldl (\acc x -> acc * 10 + x) 0

myratioWithFoldl = foldl1 (/)
