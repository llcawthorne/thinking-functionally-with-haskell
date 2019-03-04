Foxy
24th November, 2013
In Chapter 02
________________________________________________

> type CIN = [Char]

> addSum :: CIN -> CIN
> addSum cin =
>   cin ++ show (n `div` 10) ++ show (n `mod` 10)
>   where n = sum (map fromDigit cin)

> valid :: CIN -> Bool
> valid cin = cin == addSum (take 8 cin)

> fromDigit :: Char -> Int
> fromDigit c = read [c]
