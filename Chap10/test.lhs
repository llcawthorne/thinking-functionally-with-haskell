> foo = do {x <- getChar;
>           if x=='a'
>           then do {putChar 'b';
>                    putStrLn "true"}
>           else do {putChar 'c';
>                    putStrLn "false"}}
> bar = do {x <- getChar;
>           if x=='a'
>           then putStrLn "true"
>           else putStrLn "false"}