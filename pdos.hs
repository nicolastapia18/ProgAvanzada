--ejercicio 2a practico 2
hd :: [a] -> a
hd [x] = x
hd(x:xs) = x

--ejercicio 2b practico 2
tl :: [a] -> [a]
tl [] = []
tl (x:xs)=xs

--ejercicio 2c practico 2
lt :: [a] -> a
lt [x] = x
lt (x:xs) = lt xs

--ejercicio 2d practico 2
it :: [a] -> [a]
it [] = []
it [x] = []
it (x:xs) = x: (it xs)

--ejercicio 3 practica 2
max3 :: Int -> Int -> Int -> Int
max3 x y z = max z(max x y)

--ejercicio 4 practica 2
concatenar :: [a] -> [a] -> [a]
concatenar [][] = []
concatenar [] ys = ys
concatenar xs [] = xs
concatenar (x:xs) ys = x : concatenar xs ys

tomar :: Int -> [a] -> [a]
tomar n [] = []
tomar 0 xs = []
tomar n (x:xs) = x : (tomar (n-1) xs)

tirar :: Int -> [a] -> [a]
tirar 0 [] = []
tirar n [] = []
tirar 0 xs = xs
tirar n (x:xs) = tirar (n-1) xs

agFinal :: a -> [a] -> [a]
agFinal n xs = xs ++ [n]

--ejercicio 5 practica 2
abso :: Int -> Int
abso x | x >= 0 = x
       | x < 0 = x * (-1)

--ejercicio 6 practica 2
edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (d1, m1, a1) (d2, m2, a2) = a2 - a1

--ejercicio 7a practica 2
xor :: Bool -> Bool -> Bool
xor False False = False
xor _ _ = True

--ejercicio 7b practica 2
xor2 :: Bool -> Bool -> Bool
xor2 False False = False
xor2 True True = False
xor2 _ _ = True

--ejercicio 8 practica 2
prim' :: Int -> Int -> Bool
prim' x 1 = False
prim' x y = mod x y == 0 || prim' x (y-1)
primo :: Int -> Bool
primo 1 = False
primo 2 = True
primo x = not (prim' x (x-1))

lisPrimo :: Int -> [Int]
lisPrimo 0 = []
lisPrimo x | primo x = x : lisPrimo (x-1)
           | otherwise = lisPrimo (x-1)