> module Matchings (match)
> where
> import Expressions
> import Substitutions (Substitution, unitSub, combine)
> import Utilities (parts)

> match :: (Expr,Expr) -> [Substitution]
> match = concatMap (combine . map matchA) . alignments

> matchA :: (Atom,Expr) -> [Substitution]
> matchA (Var v,e) = [unitSub v e]
> matchA (Con k1 es1,Compose [Con k2 es2])
>    | k1==k2 = combine (map match (zip es1 es2))
> matchA _ = []

> alignments :: (Expr,Expr) -> [[(Atom,Expr)]]
> alignments (Compose as,Compose bs)
>       = [zip as (map Compose bss)
>         | bss <- parts (length as) bs]
