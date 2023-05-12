--ejercicio 1
data Nat = Zero | Suc Nat

instance Show Nat where
 show Zero = "Zero"
 show (Suc n) = "Suc" ++ show n

--ejercicio 2
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc x) = 1 + natToInt x

--ejercicio 3
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = Suc (intToNat (x-1))

--ejercicio 4
natSum :: Nat -> Nat -> Nat
natSum Zero n = n
natSum (Suc x) n = Suc (natSum x n)

--ejercicio 5
data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

--ejercicio 6
size :: BinTree a -> Int 
size Empty = 0
size (Node _ left right) = 1 + size left + size right

--ejercicio 7
height :: BinTree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)