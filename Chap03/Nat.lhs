Natural Numbers
20th March, 2012
In Chapter 03
_____________________________________________________________

> data Nat = Zero | Succ Nat

> instance Eq Nat where
>  Zero == Zero     = True
>  Zero == Succ n   = False
>  Succ m == Zero   = False
>  Succ m == Succ n = (m == n)

> instance Show Nat where
>  show Zero            = "Zero"
>  show (Succ Zero)     = "Succ Zero"
>  show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"

> instance Num Nat where
>   m + Zero     = m
>   m + Succ n   = Succ (m+n)

>   m * Zero     = Zero
>   m * (Succ n) = m * n + m

>   abs n = n
>   signum Zero     = Zero
>   signum (Succ n) = Succ Zero

>   m - Zero        = m
>   Zero - Succ n   = Zero
>   Succ m - Succ n = m - n

>   fromInteger x 
>     | x <= 0    = Zero
>     | otherwise = Succ (fromInteger (x-1)) 

> infinity = Succ infinity

> instance Ord Nat where
>   Zero < Zero     = False
>   Zero < Succ n   = True
>   Succ m < Zero   = False
>   Succ m < Succ n = (m < n)

> divModNat :: Nat -> Nat -> (Nat,Nat)
> divModNat x y = if x < y then (Zero,x)
>                 else (Succ q,r)
>                 where (q,r) = divModNat (x-y) y
