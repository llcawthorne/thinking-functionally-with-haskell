Pretty-Printing
2nd April, 2013
In Chapter 8

The shallow embedding:

> module PrettyShallow
>     (Doc, Layout,
>      nil, line, text, 
>      nest, (<>), group, 
>      layouts, pretty, layout)
> where

> type Layout = String
> type Doc    = [Layout]


> layout :: Layout -> IO ()
> layout = putStrLn

> layouts :: Doc -> [Layout]
> layouts = id

> nil, line :: Doc
> nil      = [""]
> line     = ["\n"]

> text :: String -> Doc
> text s   = [s]

> nest :: Int -> Doc -> Doc
> nest i x = map (nestl i) x
>  where nestl i = concat . map (indent i)
>        indent i c = if c=='\n' then c:replicate i ' '
>                     else [c]
 
> (<>) :: Doc -> Doc -> Doc
> x <> y = [lx ++ ly | lx <- x, ly <- y]

> group :: Doc -> Doc
> group x   = flatten x ++ x
> flatten x = [flattenl (head x)]

> flattenl :: Layout -> Layout
> flattenl [] = []
> flattenl (c:cs) = if c=='\n'
>                   then ' ':flattenl (dropWhile (== ' ') cs)
>                   else c:flattenl cs

> pretty :: Int -> Doc -> Layout
> pretty w = fst . foldr1 choose . map augment
>  where
>  augment lx = (lx,shape lx)
>  choose alx aly
>   = if better (snd alx) (snd aly) then alx else aly
>  better [] ks         = True
>  better js []         = False
>  better (j:js) (k:ks) | j == k    = better js ks
>                       | otherwise = (j <= w)

> shape :: Layout -> [Int]
> shape = map length . lines

