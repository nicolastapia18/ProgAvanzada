--parcial 1 ejercicio 1
data BinTree a = Empty | Node a (BinTree a) (BinTree a)

tree :: BinTree Int
tree = Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 7 (Node 6 Empty Empty) (Node 9 Empty Empty))

sumTree :: BinTree Int -> Int
sumTree Empty = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

compact :: [Int] -> [Int]
compact [] = []
compact [x] = [x]
compact (x:y:ys) | x == y = compact (y:ys)
                 | otherwise = x : compact (y:ys)

compactdos :: [Int] -> [Int]
compactdos [] = [] -- caso base: lista vacía
compactdos [x] = [x] -- caso base: lista con un solo elemento
compactdos (x:xs) = x : [y | (x,y) <- zip (x:xs) xs, x /= y] -- caso general

eliminarRepetidosEnteros :: [Int] -> [Int]
eliminarRepetidosEnteros [] = []
eliminarRepetidosEnteros (x:xs)
    | elem x xs = eliminarRepetidosEnteros xs
    | otherwise = x : eliminarRepetidosEnteros xs

list :: Int -> [Int] -> Bool
list 0 xs = False
list n [] = False
list n (x:xs) = elem x xs || list (n-1) xs

diagonal :: [(Int, Int)]
diagonal = [(i, i) | i <- [0..]]

diagonalCoords :: [(Int, Int)]
diagonalCoords = [(i-j, j) | n <- [0..], i <- [0..n], j <- [0..i]]

pits :: [(Int,Int,Int)]
pits = [(a,b,c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

plain :: [[a]] -> [a]
plain = foldl (++) []

par :: Int -> Bool
par n | mod n 2 == 0 = True
      | otherwise = False

{-superPar :: Int -> [Bool]
superPar n = [par x | x <- n]-}

numToList :: Integer -> [Integer]
numToList n | n < 0     = error "El número debe ser positivo"
            | n < 10    = [n]
            | otherwise = numToList (n `div` 10) ++ [n `mod` 10]

enRango :: Int -> Int -> [Int] -> [Int]
enRango a b xs = [x | x <- xs, x >= a, x <= b]

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False