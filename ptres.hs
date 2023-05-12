--ejercicio 1 practica 3
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

--ejercicio 2 practica 3
ordenar ::  [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = ordenar left ++ [x] ++ ordenar right
        where
            left = [a | a <- xs, a <= x]
            right = [b | b <- xs, b > x]

--ejercicio 3 practica 3
pot :: Int -> Int
pot 0 = 1
pot 1 = 2
pot n = 2 * pot (n-1)

--ejercicio 4 practica 3
bin :: Int -> [Int]
bin 0 = []
bin n | mod n 2 == 0 = bin (div n 2) ++ [0]
      | otherwise = bin (div n 2) ++ [1]

--ejercicio 5 practica 3
{-dec :: Int -> Int
dec 0 = 0
dec n = (mod n 10) + 2 * dec (div n 10)

par :: Int -> Bool
par n | mod (dec n) 2 == 0 = True
      |otherwise = False
-}
par :: Int -> Bool
par n | mod n 2 == 0 = True
      | otherwise = False

--ejercicio 6 practica 3
distanciaH :: String -> String -> Int
distanciaH xs [] = 0
distanciaH [] ys = 0
distanciaH (x:xs) (y:ys) | x == y = 0 + distanciaH xs ys
                         | otherwise = 1 + distanciaH xs ys

--ejercicio 7 practica 3
cuad :: Float -> Bool
cuad n | floor(sqrt n) == ceiling(sqrt n) = True
       | otherwise = False
       
--ejercicio 8 practica 3
repetidos :: Eq a => [a] -> a -> Int -> Bool
repetidos xs a n | cont xs a == n = True
                | otherwise = False

cont :: Eq a => [a] -> a -> Int
cont [] a = 0
cont (x:xs) a | x == a = 1 + cont xs a
              | otherwise = 0 + cont xs a

--ejercicio 9 practica 3
nelem :: Eq a => [a] -> Int -> a
nelem [] n = error "No existe"
nelem (x:xs) n | n == 0 = x
               | otherwise = nelem xs (n-1)

--ejercicio 10 practica 3
posicionesC :: String -> Char -> [Int]
posicionesC xs y = posicionesAux xs y 0

posicionesAux :: String -> Char -> Int -> [Int]
posicionesAux [] c n = []
posicionesAux (x:xs) c n | x == c = n : posicionesAux xs c (n+1)
                         | otherwise = posicionesAux xs c (n+1)