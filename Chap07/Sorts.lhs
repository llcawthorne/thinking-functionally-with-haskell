Sorting
12th March, 2012
In Chapter 7
__________________________________________________________
1. Merge sort

> msort1 :: (Ord a) => [a] -> [a]
> msort1 []  = []
> msort1 [x] = [x]
> msort1 xs  = merge (msort1 ys) (msort1 zs)
>              where (ys,zs) = halve xs

> halve xs = splitAt (length xs `div` 2) xs

> merge :: (Ord a) => [a] -> [a] -> [a]
> merge xs'@(x:xs) ys'@(y:ys)
>   | x <= y    = x:merge xs  ys'
>   | otherwise = y:merge xs' ys
> merge [] ys = ys
> merge xs [] = xs

> msort2 :: (Ord a) => [a] -> [a]
> msort2 []  = []
> msort2 [x] = [x]
> msort2 xs  = merge (msort2 ys) (msort2 zs)
>             where (ys,zs) = split xs

> split :: [a] -> ([a],[a])
> split []     = ([],[])
> split (x:xs) = (x:zs,ys)
>                where (ys,zs) = split xs

> msort3 :: (Ord a) => [a] -> [a]
> msort3 []  = []
> msort3 [x] = [x]
> msort3 xs  = fst (gsort (length xs) xs)
>    where gsort 0 xs = ([],xs)
>          gsort 1 xs = ([head xs],tail xs)
>          gsort n xs = (merge ys zs, xs'')
>              where (ys,xs')  = gsort m xs
>                    (zs,xs'') = gsort (n-m) xs'
>                    m         = n `div` 2

> msort4 :: (Ord a) => [a] -> [a]
> msort4 []  = []
> msort4 [x] = [x]
> msort4 xs  = merge (msort4 ys) (msort4 zs)
>              where (ys,zs) = (take n xs, drop n xs)
>                    n = length xs `div` 2

> msort5 :: (Ord a) => [a] -> [a]
> msort5 []  = []
> msort5 [x] = [x]
> msort5 xs  = fst (gsort (length xs) xs)
>    where gsort 0 xs = ([],xs)
>          gsort 1 xs = ([head xs],tail xs)
>          gsort n xs = ((merge $! ys) $! zs, xs'')
>              where (ys,xs')  = gsort m xs
>                    (zs,xs'') = gsort (n-m) xs'
>                    m         = n `div` 2

2. Quick sort

> qsort1 :: (Ord a) => [a] -> [a]
> qsort1 []     = []
> qsort1 (x:xs) = qsort1 [y | y <- xs, y < x] ++ [x] ++
>                 qsort1 [y | y <- xs, x <= y]

> qsort2 :: (Ord a) => [a] -> [a]
> qsort2 []     = []
> qsort2 (x:xs) = sortp xs [] []
>      where
>      sortp [] us vs     = qsort2 us ++ [x] ++ qsort2 vs
>      sortp (y:xs) us vs = if y < x
>                           then sortp xs (y:us) vs
>                           else sortp xs us (y:vs)


