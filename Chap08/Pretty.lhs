





















































































































Pretty-Printing
2nd April, 2013
In Chapter 8

> module Pretty (Doc, Layout,
>                nil, line, text, 
>                nest, (<>), group, 
>                layouts, pretty, 
>                layout, normalise,repn,doc,eq1,eq2,size) where

> type Layout = String
> data Doc    = Nil
>             | Line
>             | Text String
>             | Nest Int Doc
>             | Doc :<>: Doc
>             | Doc :+: Doc
>             deriving (Eq,Show)

> nil, line :: Doc
> nil      = Nil
> line     = Line

> text :: String -> Doc
> text s   = Text s

> nest :: Int -> Doc -> Doc
> nest i x = Nest i x

> (<>) :: Doc -> Doc -> Doc
> x <> y   = x :<>: y

> group :: Doc -> Doc
> group x  = flatten x :+: x

> flatten Nil        = Nil
> flatten Line       = Text " "
> flatten (Text s)   = Text s
> flatten (Nest i x) = flatten x
> flatten (x :<>: y) = flatten x :<>: flatten y
> flatten (x :+: y)  = flatten x

> layouts :: Doc -> [Layout]
> layouts x = layr [(0,x)]
>  where
>  layr [] = [""]
>  layr ((i,Nil):ids)      = layr ids
>  layr ((i,Line):ids)     = ['\n':replicate i ' ' ++ ls
>                            | ls <- layr ids]
>  layr ((i,Text s):ids)   = [s ++ ls | ls <- layr ids]
>  layr ((i,Nest j x):ids) = layr ((i+j,x):ids)
>  layr ((i,x :<>: y):ids) = layr ((i,x):(i,y):ids)
>  layr ((i,x :+: y):ids)  = layr ((i,x):ids) ++
>                            layr ((i,y):ids)

> pretty :: Int -> Doc -> Layout
> pretty w x = best w [(0,x)]
>  where
>  best r []                 = ""
>  best r ((i,Nil):ids)      = best r ids
>  best r ((i,Text s):ids)   = s ++ best (r-length s) ids
>  best r ((i,Line):ids)     = '\n':replicate i ' ' ++ 
>                              best (w-i) ids
>  best r ((i,Nest j x):ids) = best r ((i+j,x):ids)
>  best r ((i,x :<>: y):ids) = best r ((i,x):(i,y):ids)
>  best r ((i,x :+: y):ids)  = better r (best r ((i,x):ids))
>                                       (best r ((i,y):ids))
>  better r lx ly = if fits r lx then lx else ly
>  fits r x | r<0 = False
>  fits r []      = True
>  fits r (c:cs)  = if c == '\n' then True
>                   else fits (r-1) cs

> snake :: [Int] -> Doc -> Layout
> snake ws x = best ws [(0,x)]
>  where
>  best ws []                 = ""
>  best ws ((i,Nil):ids)      = best ws ids
>  best ws ((i,Text s):ids)   = s ++ best ws' ids
>                   where ws' = head ws-length s:tail ws 
>  best ws ((i,Line):ids)     = '\n':replicate i ' ' ++ 
>                               best ws' ids
>                   where ws' = head (tail ws)-i:tail (tail ws)
>  best ws ((i,Nest j x):ids) = best ws ((i+j,x):ids)
>  best ws ((i,x :<>: y):ids) = best ws ((i,x):(i,y):ids)
>  best ws ((i,x :+: y):ids)  = better w (best ws ((i,x):ids))
>                                        (best ws ((i,y):ids))
>                               where w = head ws
>  better r lx ly = if fits r lx then lx else ly
>  fits r x | r<0 = False
>  fits r []      = True
>  fits r (c:cs)  = if c == '\n' then True
>                   else fits (r-1) cs


> layout :: Layout -> IO ()
> layout = putStrLn

Pretty-printing documents:

> doc :: Doc -> Doc
> doc Nil        = text "Nil"
> doc Line       = text "Line"
> doc (Text s)   = text ("Text " ++ show s)
> doc (Nest i x) = text ("Nest " ++ show i) <> 
>                        text " " <> paren (doc x)
> doc (x :<>: y) = doc x <> text " :<>:" <> group (line <> 
>                  nest 3 (doc y))
> doc (x :+: y)  = paren (doc x) <> line <> text " :+: " <> group 
>                  (paren (nest 5 (doc y)))

> paren x = text "(" <> nest 1 x <> 
>           text ")"


> size :: Doc -> Int
> size Nil         = 1
> size Line        = 1
> size (Text s)    = 1
> size (Nest i x)  = 1 + size x
> size (x :<>: y)  = 1 + size x + size y
> size (x :+: y)   = 1 + size x + size y

> isize :: [(Int,Doc)] -> Int
> isize ids = sum [size x | (i,x) <- ids]


> repn = foldr1 (:+:) . map (foldr1 (:<>:))

> normalise :: Doc -> [[Doc]]
> normalise Nil        = [[]]
> normalise Line       = [[Nest 0 Line]]
> normalise (Text s)   = [[Text s]]
> normalise (Nest i x) = [hnest i xs | xs <- normalise x] 
> normalise (x :<>: y) = [xs <+> ys  | xs <- nx, ys <- ny]
>                        where nx = normalise x
>                              ny = normalise y   
> normalise (x :+: y)  = normalise x ++ normalise y

> hnest i []               = []
> hnest i (Text s:xs)      = Text s:hnest i xs
> hnest i (Nest j Line:xs) = Nest (i+j) Line:hnest i xs

> xs <+> [] = xs
> xs <+> ys 
>  = case last xs of
>      Text s -> case head ys of
>                 Text t -> init xs ++ [Text (s++t)] ++ tail ys
>                 _      -> xs ++ ys
>      Nest i Line -> case head ys of
>                      Nest j Line -> init xs ++ [Nest (i+j) Line] ++ tail ys
>                      _           -> xs ++ ys

> eq1 x y = layouts x == layouts y
> eq2 x y = normalise x == normalise y
