--ejercicio 8 practica 1
ej8 :: Int -> [Int]
ej8 x | x < 10 = [x]
      | otherwise = ej8 (div x 10) ++ [mod x 10]