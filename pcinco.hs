--ejercicio 1
listInf :: Int -> [Int]
listInf n = n : listInf(n)

--ejercicio 2
listInfNat :: Int -> [Int]
listInfNat n = n : listInfNat(n + 1)

--ejercicio 3
listNat :: Int -> [Int]
listNat n = [0..n]

--ejercicio 4
listCinco :: [Int] -> [Int] 
listCinco xs = listCinco [x | x <- xs, x >= 0]

--ejercicio 5
listCuad :: [Int] -> [Int]
listCuad xs = map (^2) xs

--ejercicio 6
divisores :: Int -> [Int]
divisores n = filter(\x -> mod n x == 0) [1..n]

--ejercicio 7
prim' :: Int -> Int -> Bool
prim' x 1 = False
prim' x y = mod x y == 0 || prim' x (y-1)

primo :: Int -> Bool
primo 1 = False
primo 2 = True
primo x = not (prim' x (x-1))

natPrimos :: [Int] -> [Int]
natPrimos (x:xs) = filter (primo) (x:xs)

--ejercicio 8
sumCuad :: [Int] -> Int
sumCuad [] = 0
sumCuad xs = sum (map (^2) xs)

--ejercicio 9
sucesor :: [Int] -> [Int]
sucesor xs = map (+1) xs

--ejercicio 10
sumar :: [Int] -> Int
sumar xs = sum xs

--ejercicio 11
facto :: Int -> Int
facto n = foldl (*) n [1..(n-1)]

--ejercicio 12
verifik :: [Bool] -> Bool
verifik (x:xs) = foldl (&&) True (x:xs)

--ejercicio 13
contar :: Int -> a -> Int
contar acc _ = acc + 1

tam :: [a] -> Int
tam [] = 0 
tam (x:xs) = foldl (contar) 0 (x:xs)

cont :: a -> Int -> Int
cont _ acc = acc + 1

tama :: [a] -> Int
tama [] = 0 
tama (x:xs) = foldr (cont) 0 (x:xs)

--ejercicio 14
suces :: [Int] -> [Int]
suces xs = [x + 1 | x <- xs]

--ejercicio 15
cuadList :: [Int] -> [Int]
cuadList xs = [x^2 | x <- xs]

--ejercicio 16
par :: Int -> Bool
par n | mod n 2 == 0 = True
      | otherwise = False

parMayores :: [Int] -> [Int]
parMayores xs = [x | x <- xs, par x, x > 10]

--ejercicio 17
divisor :: Int -> [Int]
divisor n = [x | x <- [1..n], mod n x == 0]

--ejercicio 18
todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and [ elem x ys | x <- xs ]

--ejercicio 19
primosN :: Int -> [Int]
primosN n = [x | x <- [2..n], primo x]

--ejercicio 20
prodCartesiano :: [Int] -> [Int] -> [(Int,Int)]
prodCartesiano xs ys = [(x, y) | x <- xs, y <- ys]

--ejercicio 21
ocurrencias :: Eq a => [a] -> a -> Int
ocurrencias ys x = length [y | y <- ys, x == y]

--ejercicio 22
split2 :: [a] -> [([a],[a])]
split2 xs = [(take n xs, drop n xs) | n <- [0..length xs]]

--ejercicio 23
sumaSeg :: [Int] -> Int
sumaSeg xs = sum [sum (take n xs) | n <- [1..length xs]]

--ejercicio 24
listaParesInf :: [Int]
listaParesInf = [2 * x | x <- [0..]]