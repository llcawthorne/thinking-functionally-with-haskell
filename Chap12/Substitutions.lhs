> module Substitutions
>   (Substitution, emptySub, unitSub,
>    combine, unify, apply)
> where

> import Expressions
> import Data.Maybe (fromJust)
> import Utilities (cp)

> type Substitution = [(VarName,Expr)]

> emptySub :: Substitution
> emptySub = []

> unitSub :: VarName -> Expr -> Substitution
> unitSub v e = [(v,e)]

> unify :: Substitution -> Substitution -> [Substitution]
> unify sub1 sub2 = if compatible sub1 sub2 
>                   then [union sub1 sub2]
>                   else []

> unifyAll :: [Substitution] -> [Substitution]
> unifyAll = foldr (concatMap . unify) [emptySub]

> compatible [] t = True
> compatible s [] = True
> compatible s@((v1,e1):s') t@((v2,e2):t')
>   | v1<v2  = compatible s' t
>   | v1==v2 = if e1==e2 then compatible s' t'
>              else False
>   | v1>v2  = compatible s t'

> union [] t = t
> union s [] = s
> union s@((v1,e1):s') t@((v2,e2):t')
>   | v1<v2  = (v1,e1):union s' t
>   | v1==v2 = (v1,e1):union s' t'
>   | v1>v2  = (v2,e2):union s t'

> combine :: [[Substitution]] -> [Substitution]
> combine = concatMap unifyAll . cp

> apply :: Substitution -> Expr -> Expr
> apply sub (Compose as) 
>   = Compose (concatMap (applyA sub) as)
> applyA sub (Var v)    = deCompose (binding sub v)
> applyA sub (Con k es) = [Con k (map (apply sub) es)]
    
> binding :: Substitution -> VarName -> Expr
> binding sub v = fromJust (lookup v sub)

