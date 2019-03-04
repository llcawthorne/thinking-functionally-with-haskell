> module Parsing where

> import Prelude hiding (guard, fail)
> import Data.Char

> newtype Parser a = Parser (String -> [(a,String)])

> apply :: Parser a -> String -> [(a,String)]
> apply (Parser p) s = p s

> parse :: Parser a -> String -> a
> parse p = fst . head . apply p

> instance Monad Parser where
>  return x = Parser (\s -> [(x,s)])
>  p >>= q  = Parser (\s -> [(y,s'') 
>                           | (x,s') <- apply p s, 
>                             (y,s'') <- apply (q x) s'])

> getc :: Parser Char
> getc = Parser f
>           where f []     = []
>                 f (c:cs) = [(c,cs)]

> sat :: (Char -> Bool) -> Parser Char
> sat p = do {c <- getc; guard (p c); return c} 

> guard :: Bool -> Parser ()
> guard True  = return ()
> guard False = fail

> fail :: Parser a
> fail = Parser (\s -> [])

> char :: Char -> Parser ()
> char x = do {c <- sat (==x); return ()}
    
> string :: String -> Parser ()
> string []     = return ()
> string (x:xs) = do {char x; string xs; return ()}

> upto :: Char -> Parser String
> upto c = Parser (\s ->
>           let (xs,ys) = break (==c) s in
>           if null ys then [] else [(xs,tail ys)])

> (<|>) :: Parser a -> Parser a -> Parser a
> p <|> q = Parser f
>           where f s = let ps = apply p s in
>                       if null ps then apply q s
>                       else ps
                          
> some :: Parser a -> Parser [a]
> some p = do {x <- p; xs <- many p; return (x:xs)}

> many :: Parser a -> Parser [a]
> many p = optional (some p)
    
> optional :: Parser [a] -> Parser [a]
> optional p = p <|> none
    
> none :: Parser [a]
> none = return []

> one :: Parser a -> Parser [a]
> one p = do {x <- p; return [x]}

> somewith :: Parser b -> Parser a -> Parser [a]
> somewith q p = do {x <- p;
>                    xs <- many (q >> p);
>                    return (x:xs)}

> manywith :: Parser b -> Parser a -> Parser [a]
> manywith q p = optional (somewith q p)

*****************************************************

> lower :: Parser Char
> lower = sat isLower

> lowers :: Parser String
> lowers = do {c <- lower; cs <- lowers; return (c:cs)}
>          <|> return ""

> space :: Parser ()
> space = many (sat isSpace) >> return ()

> token :: Parser a -> Parser a
> token p = space >> p

> symbol :: String -> Parser ()
> symbol xs = token (string xs)

> bracket :: Parser a -> Parser a
> bracket p = do {symbol "[";
>                 x <- p;
>                 symbol "]";
>                 return x}

> paren :: Parser a -> Parser a
> paren p = do {symbol "(";
>               x <- p;
>               symbol ")";
>               return x}

                              
> digit :: Parser Int
> digit = do {d <- sat isDigit; return (cvt d)}
>         where cvt d = fromEnum d - fromEnum '0'
                

