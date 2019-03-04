> module Utilities
> where

> compose :: [a -> a] -> a -> a
> compose = foldr (.) id

> segments :: [a] -> [([a],[a],[a])]
> segments as = [(as1,as2,as3)
>               | (as1,bs)  <- splits as,
>                 (as2,as3) <- splits bs]

> parts :: Int -> [a] -> [[[a]]]
> parts 0 [] = [[]]
> parts 0 as = []
> parts n as = [bs:bss 
>              | (bs,cs) <- splits as, 
>                bss <- parts (n-1) cs]

> splits :: [a] -> [([a],[a])]
> splits [] = [([],[])]
> splits (a:as)
>   = [([],a:as)] ++
>     [(a:as1,as2) | (as1,as2) <- splits as]

> cp :: [[a]] -> [[a]]
> cp [] = [[]]
> cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
>               where yss = cp xss 

> anyOne :: (a -> [a]) -> [a] -> [[a]]
> anyOne f []     = []
> anyOne f (x:xs) = [x':xs | x' <- f x] ++
>                   [x:xs' | xs' <- anyOne f xs]
