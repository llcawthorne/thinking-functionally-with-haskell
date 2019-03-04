Floors
16 March, 2012.
In Chapter 03
______________________________________________________

> badfloor :: Float -> Integer
> badfloor = read . takeWhile (/= '.') . show
 

> floor1 :: Float -> Integer
> floor1 x = if x < 0
>            then until (`leq` x) (subtract 1) (-1)
>            else until ( x `lt`) (+1) 1 - 1

> leq :: Integer -> Float -> Bool
> m `leq` x = fromInteger m <= x

> lt :: Float -> Integer -> Bool
> x `lt` n = x < fromInteger n

> floor2 :: Float -> Integer
> floor2 x = fst (until unit (shrink x) (bound x))
>            where unit (m,n) = (m+1 == n)

> type Interval = (Integer,Integer)

> shrink :: Float -> Interval -> Interval
> shrink x (m,n) = if p `leq` x then (p,n) else (m,p)
>                  where p = choose (m,n)

> choose :: Interval -> Integer
> choose (m,n) = (m+n) `div` 2

> bound :: Float -> Interval
> bound x = (lower x, upper x)

> lower :: Float -> Integer
> lower x = until (`leq` x) (*2) (-1)

> upper :: Float -> Integer
> upper x = until (x `lt`) (*2) 1
