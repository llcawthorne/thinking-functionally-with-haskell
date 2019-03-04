Square roots
20th March, 2012
In Chapter 03
__________________________________________________________

> import Prelude hiding (sqrt)

> type Interval = (Integer,Integer)

> isqrt :: Float -> Integer
> isqrt x = fst (until unit (shrink x) (bound x))
>           where unit (m,n) = (m+1 == n)

> shrink :: Float -> Interval -> Interval
> shrink x (m,n) = if (p*p) `leq` x then (p,n) else (m,p)
>                  where p = (m+n) `div` 2

> bound :: Float -> Interval
> bound x = (0,until above (*2) 1)
>           where above n = x `lt` (n*n)

> sqrt :: Float -> Float
> sqrt x = until goodenough improve x
>   where goodenough y = abs (y*y-x) < eps*x
>         improve y = (y+x/y)/2
>         eps = 0.000001

> leq :: Integer -> Float -> Bool
> m `leq` x = fromInteger m <= x

> lt :: Float -> Integer -> Bool
> x `lt` n = x < fromInteger n
