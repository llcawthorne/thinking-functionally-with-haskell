> module Laws
>   (Law (Law), LawName, sortLaws, law,
>    Equation, equation)
> where

> import Expressions
> import Parsing
> import Data.List (partition)

> data Law      = Law LawName Equation
> type LawName  = String
> type Equation = (Expr,Expr)

> law :: Parser Law
> law = do {name <- upto ':';
>           eqn  <- equation;
>           return (Law name eqn)}

> equation :: Parser Equation
> equation = do {e1 <- expr;
>                symbol "=";
>                e2 <- expr;
>                return (e1,e2)}

> instance Show Law where
>   showsPrec _ (Law name (e1,e2))
>     = showString name .
>       showString ": " .
>       shows e1 .
>       showString " = " .
>       shows e2

> sortLaws :: [Law] -> [Law]
> sortLaws laws = simple ++ others ++ defns
>  where
>  (simple,nonsimple) = partition isSimple laws
>  (defns,others)     = partition isDefn nonsimple

> isSimple (Law _ (Compose as1,Compose as2))
>   = length as1 > length as2
> isDefn (Law _ (Compose [Con f es],_))
>   = all isVar es
> isDefn _ = False
> isVar (Compose [Var _]) = True
> isVar _                 = False
