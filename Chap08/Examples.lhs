Examples of pretty-printing

> import PrettyDeep

1. Conditional expressions

> data CExpr = Expr String | If String CExpr CExpr

> cexpr :: CExpr -> Doc
> cexpr (Expr p) = text p
> cexpr (If p x y) 
>   = group (group (text "if " <> text p <> line <> 
>                   text "then " <> nest 5 (cexpr x)) <>
>            line <> text "else " <> nest 5 (cexpr y))

> cexpr1 (Expr p) = text p
> cexpr1 (If p x y) = group (
>                     group (group (text "if " <> text p <> line <>
>                            text "then" <> line <>
>                            cexpr1 x)) <> group (line <>
>                            text "else " <> cexpr1 y))                              
  
> ce1, ce2 :: CExpr
> ce1 = If "happy" (Expr "joyous") (Expr "miserable")
> ce2 = If "happy" (If "wealthy" (Expr "fantastic") (Expr "pleasant")) 
>                  (If "in love" (Expr "idyllic") (Expr "miserable"))
> ce3 = If "wealthy" (If "happy" (Expr "lucky you") (Expr "tough"))
>                    (If "in love" (Expr "content") (Expr "miserable"))


2. General Trees

> data GenTree a = Node a [GenTree a]

> gtree :: Show a => GenTree a -> Doc
> gtree (Node x []) = text ("Node " ++ show x ++ " []")
> gtree (Node x ts) 
>    =  text ("Node " ++ show x) <> 
>       group (nest 2 (line <> bracket ts))

> bracket :: Show a => [GenTree a] -> Doc
> bracket ts = text "[" <> nest 1 (gtrees ts) <> text "]"

> gtrees :: Show a => [GenTree a] -> Doc
> gtrees [t]    = gtree t
> gtrees (t:ts) = gtree t <> text "," <>
>                 line <> gtrees ts
                   
> gt1 = Node 1 [Node 2 [Node 7 [],Node 8 []],
>               Node 3 [Node 9 [Node 10 [],Node 11 []]],
>               Node 4 [],
>               Node 5 [Node 6 []]]
> gt2 = Node 1 [Node 2 [Node 7 [],Node 8 []]]
> gt3 = Node 2 [Node 7 [],Node 8 []]

3. Paragraphs

> para :: String -> Doc
> para = cvt . map text . words
>  where
>  cvt []     = nil
>  cvt (x:xs) = x <> foldr (<>) nil [group (line <> x) | x <- xs]

> pg1 = "This is a paragraph of a number of lines. Each line is going to be " ++ 
>       "pretty printed."
> pg2 = "This is a short paragraph with a number of lines of fairly short words. " ++
>       "Each line is going to be pretty printed within a total line width of " ++ 
>       "length w. The process will be greedy: if the next word fits it will " ++
>       "be placed on the current line. Otherwise a new line is started."
> pg3 = "This is a fairly short paragraph with just twenty-two words. The problem is that pretty-printing it takes time, in fact 31.32 seconds."
> pg4 = "A lost and lonely hippopotamus went into a bar."

> prettybad w = fst . choose . map augment . layouts
>  where augment lx = (lx,shape lx)
>        choose as = if null bs then last as else head bs
>                    where bs = filter (fits w) as
>        fits w (lx,js) = all (<=w) js

> shape = map length . lines 

> egotist :: Int -> Doc
> egotist n | n==0  = text "1"
>          | otherwise = egotist (n-1) <> text "1"

> egoist :: Int -> Doc
> egoist n  | n==0  = text "1"
>          | otherwise = nest 1 (text "1" <> egoist (n-1))

> sgroup (xs:xss)   = [sum xs + length xs - 1]:xs:xss
> snest i           = map (add i)
> add i (x:xs)      = x: [i+y | y <- xs]
> addspace i (x:xs) = x:[replicate i ' ' ++ x | x <- xs]
> msml              = map (map length . mylines) . layouts
> msl               = map (map length . lines) . layouts

> mylines xs = ys:if null zs then [] else mylines (tail zs)
>              where (ys,zs) = break (=='\n') xs

> join lxs lys = [glue lx ly | lx <- lxs, ly <- lys]
>  where glue lx ly = init lx ++ [last lx + head ly] ++ tail ly 

The following shows mylines is required:

*Main> join (msl (text "hallo" <> line)) (msl (text "there"))
[[10]]
(0.00 secs, 1583504 bytes)
*Main> join (msml (text "hallo" <> line)) (msl (text "there"))
[[5,5]]
(0.02 secs, 1583512 bytes)
*Main> msl (text "hallo" <> line <> text "there")
[[5,5]]
(0.02 secs, 2104892 bytes)
*Main> msml (text "hallo" <> line <> text "there")
[[5,5]]
(0.00 secs, 1581352 bytes)
*Main> 

we have msml . group = sgroup . msml
        msml . nest i = snest i . msml
        msml (x <> y) = join (msl x) (msl y)
        
both snest i and sgroup are monotonic,
and if x and y are monotonic, then so is x <+> y.

[[Text "if p then if q then x else y else z"],
 [Text "if p then if q then x else y",Nest 0 Line,Text "else z"],
 [Text "if p",Nest 0 Line,Text "then if q then x else y",Nest 0 Line,Text "else z"],
 [Text "if p",Nest 0 Line,Text "then if q then x",Nest 5 Line,Text "else y",Nest 0 Line,
  Text "else z"],
 [Text "if p",Nest 0 Line,Text "then if q",Nest 5 Line,Text "then x",Nest 5 Line,
  Text "else y",Nest 0 Line,Text "else z"]]


> displayall xss = putStrLn (concat [ xs ++ "\n\n" | xs <- xss])

[[Text "if p then if q then a else b else if r then c else d"],
 [Text "if p then if q then a else b",Nest 0 Line,Text "else if r then c else d"],
 [Text "if p then if q then a else b",Nest 0 Line,Text "else if r then c",Nest 5 Line,
   Text "else d"],
 [Text "if p then if q then a else b",Nest 0 Line,Text "else if r",Nest 5 Line,
   Text "then c",Nest 5 Line,Text "else d"],
 [Text "if p",Nest 0 Line,Text "then if q then a else b",Nest 0 Line,
   Text "else if r then c else d"],
 [Text "if p",Nest 0 Line,Text "then if q then a else b",Nest 0 Line,
   Text "else if r then c",Nest 5 Line,Text "else d"],
 [Text "if p",Nest 0 Line,Text "then if q then a else b",Nest 0 Line,
   Text "else if r",Nest 5 Line,Text "then c",Nest 5 Line,Text "else d"],
 [Text "if p",Nest 0 Line,Text "then if q then a",Nest 5 Line,Text "else b",
   Nest 0 Line,Text "else if r then c else d"],
 [Text "if p",Nest 0 Line,Text "then if q then a",Nest 5 Line,Text "else b",Nest 0 Line,
   Text "else if r then c",Nest 5 Line,Text "else d"],
 [Text "if p",Nest 0 Line,Text "then if q then a",Nest 5 Line,Text "else b",Nest 0 Line,
   Text "else if r",Nest 5 Line,Text "then c",Nest 5 Line,Text "else d"],
 [Text "if p",Nest 0 Line,Text "then if q",Nest 5 Line,Text "then a",Nest 5 Line,
   Text "else b",Nest 0 Line,Text "else if r then c else d"],
 [Text "if p",Nest 0 Line,Text "then if q",Nest 5 Line,Text "then a",Nest 5 Line,
   Text "else b",Nest 0 Line,Text "else if r then c",Nest 5 Line,Text "else d"],
 [Text "if p",Nest 0 Line,Text "then if q",Nest 5 Line,Text "then a",Nest 5 Line,
   Text "else b",Nest 0 Line,Text "else if r",Nest 5 Line,Text "then c",Nest 5 Line,
   Text "else d"]]
