> module Calculations 
>   (Calculation (Calc), Step,
>    paste, calculate)
> where

> import Expressions
> import Laws
> import Rewrites
> import Utilities (compose)

> data Calculation = Calc Expr [Step]
> type Step        = (LawName,Expr)

> calculate :: [Law] -> Expr -> Calculation 
> calculate laws e = Calc e (manyStep rws e)
>  where rws e = [(name,e')
>                | Law name eqn <- sortLaws laws,
>                  e' <- rewrites eqn e, e' /= e]

> manyStep :: (Expr -> [Step]) -> Expr -> [Step]
> manyStep rws e
>   = if null steps then []
>     else step : manyStep rws (snd step)
>     where steps = rws e
>           step  = head steps

> instance Show Calculation where
>   showsPrec _ (Calc e steps)
>     = showString "\n  " .
>       shows e .
>       showChar '\n' .
>       compose (map showStep steps)

> showStep :: Step -> ShowS
> showStep (why,e)
>   = showString "=   {" .
>     showString why .
>     showString "}\n  " .
>     shows e .
>     showChar '\n'

> reverseCalc :: Calculation -> Calculation
> reverseCalc (Calc e steps)
>   = foldl shunt (Calc e []) steps
>     where
>     shunt (Calc e1 steps) (why,e2)
>       = Calc e2 ((why,e1):steps)

> paste :: Calculation -> Calculation -> Calculation
> paste calc1@(Calc e1 steps1) calc2
>    = if conc1 == conc2
>      then Calc e1 (prune conc1 rsteps1 rsteps2)
>      else Calc e1 (steps1 ++ (gap,conc2):rsteps2)
>      where Calc conc1 rsteps1 = reverseCalc calc1
>            Calc conc2 rsteps2 = reverseCalc calc2
>            gap = "... ??? ..."

> prune :: Expr -> [Step] -> [Step] -> [Step]
> prune e ((_,e1):steps1) ((_,e2):steps2)
>   | e1==e2 = prune e1 steps1 steps2
> prune e steps1 steps2 = rsteps ++ steps2
>   where Calc _ rsteps = reverseCalc (Calc e steps1)
