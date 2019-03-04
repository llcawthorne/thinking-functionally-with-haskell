Parsing
30th September, 2013
In Chapter 11

> import Prelude hiding (guard, fail)
> import Data.Char

> newtype Parser a = Parser (String -> [(a,String)])

> apply :: Parser a -> String -> [(a,String)]
> apply (Parser p) s = p s

> parse :: Parser a -> String -> a
> parse p = fst . head . apply p
    
> instance Monad Parser where
>  return x = Parser (\s -> [(x,s)])
>  p >>= q  = Parser (\s -> [(y,s'') | 
>                            (x,s') <- apply p s, 
>                            (y,s'') <- apply (q x) s'])

> getc :: Parser Char
> getc = Parser f
>        where f []     = []
>              f (c:cs) = [(c,cs)]

> sat :: (Char -> Bool) -> Parser Char
> sat p = do {c <- getc; guard (p c); return c} 

> guard :: Bool -> Parser ()
> guard True  = return ()
> guard False = Main.fail

> fail :: Parser a
> fail = Parser (\s -> [])

> char :: Char -> Parser ()
> char x = do {c <- sat (==x); return ()}
    
> string :: String -> Parser ()
> string []     = return ()
> string (x:xs) = do {char x; string xs; return ()}
    
> digit :: Parser Int
> digit = do {d <- sat isDigit; return (cvt d)}
>         where cvt d = fromEnum d - fromEnum '0'

> lower :: Parser Char
> lower = sat isLower

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

> somewith :: Parser b -> Parser a -> Parser [a]
> somewith q p = do {x <- p;
>                    xs <- many (q >> p);
>                    return (x:xs)}

> manywith :: Parser b -> Parser a -> Parser [a]
> manywith q p = optional (somewith q p)

*****************************************************

> space :: Parser ()
> space = many (sat isSpace) >> return ()

> token :: Parser a -> Parser a
> token p = space >> p

> symbol :: String -> Parser ()
> symbol xs = space >> string xs 

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

> natural :: Parser Int
> natural = space >> nat

> nat = do {ds <- some digit;
>           return (foldl1 plus ds)}
>       where m `plus` n = 10*m+n

> int0 :: Parser Int
> int0 = do {symbol "-"; n <- nat; return (-n)}
>        <|> natural

> int :: Parser Int
> int = do {space; f <- minus; n <- nat; return (f n)}
>   where 
>   minus = (char '-' >> return negate) <|> return id

> ints :: Parser [Int]
> ints = bracket (manywith (symbol ",") int)

> float :: Parser Float
> float = do {ds <- some digit; 
>             char '.'; 
>             fs <- some digit;
>             return (foldl shiftl 0 ds +
>                     foldr shiftr 0 fs)}
>         where shiftl n d = 10*n + fromIntegral d
>               shiftr f x = (fromIntegral f+x)/10

***********************************************

> data Expr = Con Int | Bin Op Expr Expr  --- deriving Show
> data Op   = Plus | Minus | Mul | Div    --- deriving Show

Fully-parenthesised, plus and minus only:

> expr1 :: Parser Expr
> expr1 = token (constant <|> paren binary)
>  where
>  binary = do {e1 <- expr1;
>               p <- op;
>               e2 <- expr1;
>               return (Bin p e1 e2)}

> op = (symbol "+" >> return Plus) <|>
>      (symbol "-" >> return Minus)

> constant = do {n <- nat; return (Con n)}

> expr0 :: Parser Expr
> expr0 = token (term <|> paren binary)
>  where
>  term = token (constant <|> paren expr0)
>  binary = do {e1 <- expr0;
>               p <- op;
>               e2 <- expr0;
>               return (Bin p e1 e2)}


Not fully parenthesized:
Black hole:

> expr2 :: Parser Expr
> expr2 = token (binary <|> term)
>  where
>  binary = do {e1 <- expr2;
>               p  <- op;
>               e2 <- term;
>               return (Bin p e1 e2)}
>  term = token (constant <|> paren expr2)

Another incorrect implementation, recognising only a term:

> expr3 :: Parser Expr
> expr3 = token (term <|> binary)
>  where
>  binary = do {e1 <- expr3;
>               p  <- op;
>               e2 <- term;
>               return (Bin p e1 e2)}
>  term = token (constant <|> paren expr3)

Best:

> expr4 :: Parser Expr
> expr4 = token (term >>= rest)
>  where
>  rest e1 = do {p <- op;
>                e2 <- term;
>                rest (Bin p e1 e2)}
>            <|> return e1
>  term = token (constant <|> paren expr4)

Allowing mul and div:

> expr5 :: Parser Expr
> expr5 = token (term >>= rest)
>  where
>  rest e1 = do {p <- addop;
>                e2 <- term;
>                rest (Bin p e1 e2)}
>            <|> return e1
>  term = token (factor >>= more)
>  more e1 = do {p <- mulop;
>                e2 <- factor;
>                more (Bin p e1 e2)}
>            <|> return e1
>  factor = token (constant <|> paren expr5)
>  addop = (symbol "+" >> return Plus) <|>
>          (symbol "-" >> return Minus)
>  mulop = (symbol "*" >> return Mul) <|>
>          (symbol "/" >> return Div)

> expr6 :: Parser Expr
> expr6 = do {e1 <- factor;
>             pes <- many (pair op factor);
>             return (foldl shunt e1 pes)}
>  where
>  shunt e1 (p,e2) = Bin p e1 e2
>  factor = token (constant <|> paren binary)
>  binary = do {e1 <- expr6;
>               p  <- op;
>               e2 <- factor;
>               return (Bin p e1 e2)}

> pair :: Parser a -> Parser b -> Parser (a,b)
> pair p q = do {x <- p; y <- q; return (x,y)}

*********************************************
Showing fully parenthesized expressions:

> showexpr1 :: Expr -> String
> showexpr1 (Con n) = show n
> showexpr1 (Bin op e1 e2)
>   = "(" ++ showexpr1 e1 ++ " " ++ showop op ++ " " ++
>      showexpr1 e2 ++ ")"
> showop Plus = "+"
> showop Minus = "-"
> showop Mul = "*"
> showop Div = "/"

More efficient version:

> showexpr2 :: Expr -> String
> showexpr2 e = showsexpr e ""
>  where
>  showsexpr (Con n) = showString (show n)
>  showsexpr (Bin op e1 e2)
>   = showParen True (showsexpr e1 . showSpace. 
>     showsop op . showSpace  . showsexpr e2)

> showsop Plus  = showChar '+'
> showsop Minus = showChar '-'
> showsop Mul   = showChar '*'
> showsop Div   = showChar '/'
> showSpace     = showChar ' '


Not fully parenthesized, + and - only:

> showexpr3 :: Expr -> String
> showexpr3 e = showsexpr False e ""
>  where
>  showsexpr _ (Con n) = showString (show n)
>  showsexpr p (Bin op e1 e2)
>   = showParen p (showsexpr False e1 . showChar ' ' . 
>     showsop op . showChar ' '  . showsexpr True e2)

Not fully parenthesize, all 4 ops

> showexpr4 :: Expr -> String
> showexpr4 e = showsPrec (const False) e ""
>  where
>  showsPrec _ (Con n) = showString (show n)
>  showsPrec f (Bin op e1 e2)
>   = showParen (f op) (showsPrec f1 e1 . showSpace . 
>     showsop op . showSpace . showsPrec f2 e2)
>     where f1 x = isMulOp op && isAddOp x
>           f2 x = isMulOp op || isAddOp x

> isMulOp Mul = True
> isMulOp Div = True
> isMulOp _   = False
> isAddOp = not . isMulOp

> showexpr5 :: Expr -> String
> showexpr5 e = showsPrec (0>) e ""
>  where
>  showsPrec _ (Con n) = showString (show n)
>  showsPrec f (Bin op e1 e2)
>   = showParen (f p) (showsPrec (p>) e1 . showSpace . 
>     showsop op . showSpace . showsPrec (p>=) e2)
>     where p = prec op

> prec Mul = 2
> prec Div = 2
> prec Plus = 1
> prec Minus = 1

> showexpr6 :: Expr -> String
> showexpr6 e = showsPrec 0 e ""
>  where
>  showsPrec _ (Con n) = showString (show n)
>  showsPrec p (Bin op e1 e2)
>   | isMulOp op = showParen (p>2) (showsPrec 2 e1 . showSpace . 
>     showsop op . showSpace . showsPrec 3 e2)
>   | otherwise  = showParen (p>1) (showsPrec 1 e1 . showSpace .
>     showsop op . showSpace . showsPrec 2 e2)

> instance Show Expr where
>  showsPrec _ (Con n) = showString (show n)
>  showsPrec p (Bin op e1 e2)
>   = showParen (p>q) (showsPrec q e1 . showSpace . 
>     showsop op . showSpace . showsPrec (q+1) e2)
>     where q = prec op

> showexpr7 :: Expr -> String
> showexpr7 e = showsF (const False) e ""
>   where
>   showsF _ (Con n) = showString (show n)
>   showsF f (Bin op e1 e2)
>      = showParen (f op) (showsF f1 e1 . showSpace . 
>        showsop op . showSpace . showsF f2 e2)
>        where f1 x = isMulOp op && isAddOp x
>              f2 x = isMulOp op || isAddOp x
  

