Paper-Rock-Scissors
4th October, 2012
In Chapter 09

> import System.Random

> data Move  = Paper | Rock | Scissors deriving Show
> type Round = (Move,Move)

> score :: Round -> (Int,Int)
> score (x,y) | x `beats` y  = (1,0)
>             | y `beats` x  = (0,1)
>             | otherwise    = (0,0)

> Paper `beats` Rock     = True
> Rock `beats` Scissors  = True
> Scissors `beats` Paper = True
> _ `beats` _            = False

> type Strategy1 = [Move] -> Move

> copy1,smart1 :: Strategy1
> copy1 ms  = if null ms then Rock else head ms
> smart1 ms = if null ms then Rock else pick (stats ms)

> stats :: [Move] -> (Int,Int,Int)
> stats = foldr count (0,0,0)
> count Paper (p,r,s) = (p+1,r,s)
> count Rock  (p,r,s) = (p,r+1,s)
> count Scissors (p,r,s) = (p,r,s+1)

> pick :: (Int,Int,Int) -> Move
> pick (p,r,s)
>       | m < p     = Scissors
>       | m < p+r   = Paper
>       | otherwise = Rock
>       where m = rand (p+r+s)

> rand :: Int -> Int
> rand n = fst $ randomR (0,n-1) (mkStdGen n)

> rounds1 :: (Strategy1,Strategy1) -> [Round]
> rounds1 (p1,p2) 
>    = map head $ tail $ iterate (extend (p1,p2)) []
> extend (p1,p2) rs 
>    = (p1 (map snd rs),p2 (map fst rs)):rs

> match1 :: Int -> (Strategy1,Strategy1) -> (Int,Int)
> match1 n = total . map score . take n . rounds1
>           where total xs = (sum (map fst xs),sum (map snd xs))

*****************************************************************

> type Strategy2 = [Move] -> [Move]

> copy2, smart2 :: Strategy2
> copy2 ms  = Rock:ms
> smart2 ms = Rock:map pick (stats ms)
>   where stats = tail . scanl (flip count) (0,0,0)

> rounds2 :: (Strategy2,Strategy2) -> [Round]
> rounds2 (p1,p2) = zip xs ys
>                   where xs = p1 ys
>                         ys = p2 xs

> match2 :: Int -> (Strategy2,Strategy2) -> (Int,Int)
> match2 n = total . map score . take n . rounds2
>            where total xs = (sum (map fst xs),sum (map snd xs))

> cheat ms     = map trump ms
> devious n ms = take n (copy2 ms) ++ cheat (drop n ms)

> trump Rock = Paper
> trump Paper = Scissors
> trump Scissors = Rock

> rounds3 :: (Strategy2,Strategy2) -> [Round]
> rounds3 (p1,p2) = zip xs ys
>                   where xs = police p1 ys
>                         ys = police p2 xs

> police p ms = ms'
>  where ms' = p (synch ms ms')

> synch (x:xs) (y:ys) = (y `seq` x):synch xs ys

> match3 :: Int -> (Strategy2,Strategy2) -> (Int,Int)
> match3 n = total . map score . take n . rounds3
>            where total xs = (sum (map fst xs),sum (map snd xs))



