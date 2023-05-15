--PRACTICO 2------------------------------------------------
hd :: [a] -> a
hd (x:xs) = x

tl :: [a] -> [a]
tl (x:xs) = xs

lt :: [a] -> a
lt [x] = x
lt (x:xs) = lt xs

it :: [a] -> [a]
it [] = []
it [x] = []
it (x:xs) = x : it xs

maxTres :: Int -> Int -> Int -> Int
maxTres x y z | x > y && x > z = x
              | y > x && y > z = y
              | otherwise = z

concatenar :: [a] -> [a] -> [a]
concatenar [] [] = []
concatenar [] ys = ys
concatenar xs [] = xs
concatenar (x:xs) ys = x : concatenar xs ys

tomar :: Int -> [a] -> [a]
tomar n [] = []
tomar 0 xs = []
tomar n (x:xs) = x : tomar (n-1) xs

tirar :: Int -> [a] -> [a]
tirar n [] = []
tirar 0 xs = xs
tirar n (x:xs) = tirar (n-1) xs

agFin :: a -> [a] -> [a]
agFin x ys = ys ++ [x]

abso :: Int -> Int
abso x | x >= 0 = x
      | otherwise = x * (-1)

edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (d1, m1, a1) (d2, m2, a2) = a2 - a1

xor :: Bool -> Bool -> Bool
xor False False = False
xor _ _ = True
xor2 :: Bool -> Bool -> Bool
xor2 False True = True
xor2 True False = True
xor2 _ _ = False

prim' :: Int -> Int -> Bool
prim' x 1 = False
prim' x y = mod x y == 0 || prim' x (y-1)
primo :: Int -> Bool
primo 1 = False
primo 2 = True
primo x = not (prim' x (x-1))

listPrimos :: Int -> [Int]
listPrimos 0 = []
listPrimos x | primo x = x : listPrimos (x-1)
             | otherwise = listPrimos (x-1)

alvere :: [a] -> [a]
alvere [] = []
alvere (x:xs) = alvere xs ++ [x]

iguales :: Eq a => [a] -> [a] -> Bool
iguales [] [] = True
iguales [] _ = False
iguales _ [] = False
iguales (x:xs) (y:ys) = (x == y) && iguales xs ys

palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

--PRACTICO 3-------------------------------------------------
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

ordenar ::  [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = ordenar left ++ [x] ++ ordenar right
        where
            left = [a | a <- xs, a <= x]
            right = [b | b <- xs, b > x]

pot :: Int -> Int
pot 0 = 1
pot n = 2 * pot (n-1)

natABin :: Int -> [Int]
natABin 0 = []
natABin n | mod n 2 == 0 = natABin (div n 2) ++ [0]
          | otherwise = natABin (div n 2) ++ [1]

par :: Int -> Bool
par n | mod n 2 == 0 = True
      | otherwise = False

distanciaH :: String -> String -> Int
distanciaH xs [] = 0
distanciaH [] ys = 0
distanciaH (x:xs) (y:ys) | x == y = 0 + distanciaH xs ys
                         | otherwise = 1 + distanciaH xs ys

cuad :: Float -> Bool
cuad n | floor(sqrt n) == ceiling(sqrt n) = True
       | otherwise = False

repetidos :: Eq a => [a] -> a -> Int -> Bool
repetidos _ _ 0 = True
repetidos [] _ _ = False
repetidos (z:zs) n x | z == n = repetidos zs n (x-1)
                     | otherwise = repetidos zs n x

nElem :: [a] -> Int -> a
nElem [] n = error "No existe ese elemento"
nElem (x:xs) 0 = x
nElem (x:xs) n = nElem xs (n-1)

posicionesC :: String -> Char -> [Int]
posicionesC xs y = posicionesAux xs y 0

posicionesAux :: String -> Char -> Int -> [Int]
posicionesAux [] c n = []
posicionesAux (x:xs) c n | x == c = n : posicionesAux xs c (n+1)
                         | otherwise = posicionesAux xs c (n+1)

compact :: Eq a => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:ys) | x == y = compact(y:ys)
                 | otherwise = x : compact(y:ys)

--PRACTICO 5-------------------------------------------------
unosInf :: [Int]
unosInf = repeat 1

numInf :: Int -> [Int]
numInf x = x : numInf(x+1)

numNInf :: Int -> [Int]
numNInf n = [0..n]

primCinco :: [Int] -> [Int]
primCinco xs = [x | x <- xs, x <= 5]

listCuad :: [Int] -> [Int]
listCuad xs = map (^2) xs

divisores :: Int -> [Int]
divisores n = filter(\x -> mod n x == 0) [1..n]

listPrim :: [Int] -> [Int]
listPrim (x:xs) = filter (primo) (x:xs)

sumCuad :: [Int] -> Int
sumCuad xs = sum (map (^2) xs)

sucesores :: [Int] -> [Int]
sucesores xs = map (+1) xs

sumElem :: [Int] -> Int
sumElem xs = sum xs

factorial :: Int -> Int
factorial n = foldl (*) n [1..(n-1)]

verifica :: [Bool] -> Bool
verifica (x:xs) = foldl (&&) True (x:xs)

contar :: a -> Int -> Int
contar _ acc = acc + 1
tam :: [a] -> Int
tam (x:xs) = foldr (contar) 0 (x:xs)

suce :: [Int] -> [Int]
suce xs = [x + 1 | x <- xs]

lisCuad :: [Int] -> [Int]
lisCuad xs = [x^2 | x <- xs]

paresDiez :: [Int] -> [Int]
paresDiez xs = [x | x <- xs, x > 10, mod x 2 == 0]

divis :: Int -> [Int]
divis n = [x | x <- [1..n], mod n x == 0]

todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and[elem x ys | x <- xs]

lisPrimo :: Int -> [Int]
lisPrimo n = [x | x <- [2..n], primo x]

cartesiano :: [Int] -> [Int] -> [(Int,Int)]
cartesiano xs ys = [(x,y) | x <- xs, y <- ys]

ocurrencias :: Eq a => [a] -> a -> Int
ocurrencias xs c = length[x | x <- xs, c == x]

split2 :: [a] -> [([a],[a])]
split2 xs = [(take n xs, drop n xs) | n <- [0..length xs]]

sumaSeg :: [Int] -> Int
sumaSeg xs = sum[sum(take n xs) | n <- [1..length xs]]

listParesI :: [Int]
listParesI = [x | x <- [0..], mod x 2 == 0]

--PRACTICO 6-------------------------------------------------
data Nat = Cero | Suc Nat

instance Show Nat where
 show Cero = "Cero"
 show (Suc n) = "Suc" ++ show n

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt x

intToNat :: Int -> Nat
intToNat 0 = Cero
intToNat x = Suc (intToNat (x-1))

natSum :: Nat -> Nat -> Nat
natSum Cero n = n
natSum (Suc x) n = Suc (natSum x n)

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

size :: BinTree a -> Int 
size Empty = 0
size (Node _ left right) = 1 + size left + size right

height :: BinTree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)