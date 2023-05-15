--ejercicio 1
nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

--ejercicio 2
maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj True _ True = True
maj _ True True = True
maj _ _ _ = False

--ejercicio 3
existe :: (Int -> Bool) -> [a] -> Bool
existe p xs = any p [0 .. length xs - 1]

paraTodo :: (Int -> Bool) -> [a] -> Bool
paraTodo p xs = all p [0 .. length xs - 1]

-- Predicado de ejemplo: verifica si un número es par
esPar :: Int -> Bool
esPar x = x `mod` 2 == 0
-- Lista de ejemplo
numeros :: [Int]
numeros = [1, 2, 3, 4, 5, 6]
num :: [Int]
num = [2, 4, 6, 8]
-- Ejemplo de cuantificador existencial
ejemploExistencial :: Bool
ejemploExistencial = existe (\i -> esPar (numeros !! i)) numeros
-- Ejemplo de cuantificador universal
ejemploUniversal :: Bool
ejemploUniversal = paraTodo (\i -> esPar (num !! i)) num

--ejercicio 4
sumatoria :: Num a => (a -> Bool) -> [a] -> a
sumatoria p xs = sum [x | x <- xs, p x]

productoria :: Num a => (a -> Bool) -> [a] -> a
productoria p xs = product [x | x <- xs, p x]

contatoria :: (a -> Bool) -> [a] -> Int
contatoria p xs = length [x | x <- xs, p x]

--ejemplo sumatoria
ejemSum :: Int
ejemSum = sumatoria esPar numeros
--ejemplo productoria
ejemProd :: Int
ejemProd = productoria esPar numeros
--ejemplo contatoria
ejemCon :: Int
ejemCon = contatoria esPar numeros