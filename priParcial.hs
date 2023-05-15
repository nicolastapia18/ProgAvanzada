xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

data Lista a = Nil | Cons a (Lista a)

instance (Show a) => Show (Lista a) where
    show Nil = "Nil"
    show (Cons x xs) = show x ++ " , " ++ show xs

instance (Eq a) => Eq (Lista a) where
    Nil == Nil = True
    Cons x xs == Cons y ys = x == y && xs == ys
    _ == _ = False

lisn = Nil
lisuno = Cons 1 (Cons 2 (Cons 3 (Nil)))
lisdos = Cons 13 (Cons 3 (Cons 8 (Nil)))

hd :: Lista a -> a
hd Nil = error "Lista Vacia"
hd (Cons x xs) = x

lt :: Lista a -> Lista a
lt Nil = error "Lista Vacia"
lt (Cons x xs) = xs

conca :: Lista a -> Lista a -> Lista a
conca Nil ys = ys
conca (Cons x xs) ys = Cons x (conca xs ys)

enRango :: Int -> Int -> [Int] -> [Int]
enRango a b xs = [x | x <- xs, x >= a, x <= b]

{-superPar :: Int -> Bool
superPar n | par n == True = and [par(read [x]) | x <- show n]
           | otherwise = False-}

superPar :: Int -> Bool
superPar n = and (map par (digitos n))

par :: Int -> Bool
par x = mod x 2 == 0

digitos :: Int -> [Int]
digitos n | n < 0     = error "El número debe ser positivo"
          | n < 10    = [n]
          | otherwise = digitos (n `div` 10) ++ [n `mod` 10]