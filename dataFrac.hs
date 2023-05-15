data Racional = Fraccion Int Int

instance Show Racional where
   show (Fraccion num dem) = show num ++ "/" ++ show dem

instance Eq Racional where
  (Fraccion numa dema) == (Fraccion numb demb) = numa == numb && dema == demb

frac = Fraccion 5 6
fracd = Fraccion 5 6