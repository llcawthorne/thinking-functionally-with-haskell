> module Main where

> import Expressions
> import Parsing
> import Laws
> import Calculations

> simplify :: [String] -> String -> Calculation
> simplify strings string
>  = let laws = map (parse law) strings
>        e = parse expr string 
>    in calculate laws e

> prove :: [String] -> String -> Calculation
> prove strings string 
>  = let laws    = map (parse law) strings
>        (e1,e2) = parse equation string 
>    in  paste (calculate laws e1) (calculate laws e2)

*******************************************************************
Example 1

> laws1 = ["defn filter: filter p = concat . map (box p)",
>          "defn box:    box p = if p one nil",
>          "if after dot: if p f g . h = if (p.h) (f.h) (g.h)",
>          "dot after if: h . if p f g = if p (h.f) (h.g)",
>          "nil constant: nil . f = nil",
>          "map after nil: map f . nil = nil",
>          "map after one: map f . one = one . f",
>          "map after concat: map f . concat = concat . map (map f)",
>          "map functor: map f . map g = map (f.g)",
>          "map functor: map id = id"]  

> thm1 = "filter p . map f = map f . filter (p . f)"

*****************************************************************************
Example 2

> laws2 = ["defn pruneBy: pruneBy f = f . map pruneRow . f",
>          "expand after boxs: expand . boxs = map boxs . expand",
>          "filter with boxs: filter (p . boxs) = map boxs . filter p . map boxs",
>          "boxs involution: boxs . boxs = id",
>          "map functor: map f . map g = map (f.g)",
>          "map functor: map id = id",
>          "defn expand: expand = cp . map cp",
>          "filter after cp: filter (all p) . cp = cp . map (filter p)",
>          "property of pruneRow: filter nodups . cp . pruneRow = filter nodups . cp"]

> laws2' = laws2 ++
>          ["hack: map boxs . cp . map cp = cp . map cp . boxs"]
        
> thm2 = "filter (all nodups . boxs) . expand . pruneBy boxs" ++ 
>        "= filter (all nodups . boxs) . expand"

*****************************************************************************
Example 3

> laws3 = ["defn xmatch: xmatch s = cmap (unify s) . match",
>          "unify of empty:  unify emptySub = one",
>          "cmap of one:     cmap one = id"]
> thm3 = "xmatch emptySub = match"

> laws4 = ["defn match:      match = cmap matchesA . alignments",
>          "defn xmatch:     xmatch s = cmap (unify s) . match",
>          "defn matchesA:   matchesA = combine . map matchA",
>          "defn xmatchesA:  xmatchesA s = cmap (unify s) . matchesA",
>          "cmap after cmap: cmap f . cmap g = cmap (cmap f . g)"]
> thm4 = "xmatch s = cmap (xmatchesA s) . alignments"

*****************************************************************************

> laws5 = defns ++
>         ["cmap after cmap: cmap f . cmap g = cmap (cmap f . g)"]
> laws5x = laws5 ++
>          ["cross bifunctor: (f * g) . (h * k) = (f . h) * (g . k)",
>           "cmap-cup law: cmap (cup . (one * g)) . cpp = cup . (id * cmap g)"]
> thm5 = "xmatch = cmap xmatchesA . cpp . (one * alignments)"

*****************************************************************************

> laws6 = ["defn cup: cup = cmap unify . cpp",
>          "cmap after cmap: cmap f . cmap g = cmap (cmap f . g)",
>          "cmap-cpp law: cpp . (id * cmap f) = cmap (cpp . (one * f)) . cpp"] 
> thm6  = "cmap (cup . (one * g)) . cpp = cup . (id * cmap g)"

*****************************************************************************

> defns = ["defn match:      match = cmap matchesA . alignments",
>          "defn matchesA:   matchesA = combine . map matchA",
>          "defn xmatch:     xmatch    = cup . (one * match)",
>          "defn xmatchesA:  xmatchesA = cup . (one * matchesA)",
>          "defn xmatchA:    xmatchA   = cup . (one * matchA)",
>          "defn combine:    combine = cmap unifyAll . cp"]

> cmaps = ["cmap after map:    cmap f . map g = cmap (f . g)",
>          "cmap after concat: cmap f . concat = cmap (cmap f)",
>          "cmap after nil:    cmap f . nil = nil",
>          "cmap after one:    cmap f . one = f", 
>          "cmap after cmap:   cmap f . cmap g = (cmap f . g)",
>          "cmap-cpp law: cpp . (id * cmap g) = cmap (cpp . (one * g)) . cpp",
>          "cmap-cup law: cmap (cup . (one * g)) . cpp = cup . (id * cmap g)"]

> maps  = ["map after nil: map f . nil = nil",
>          "map after one: map f . one = one . f",
>          "map after cons: map f . cons = cons . (f * map f)",
>          "map after concat: map f . concat = concat . map (map f)",
>          "map functor: map f . map g = map (f.g)",
>          "map functor: map id = id"]

> cups  = [-- "defn cup:  cup = cmap unify . cpp",
>          "cup assoc: cup . (id * cup) = cup . (cup * id) . assocl",
>          "cup ident: cup . (f * (one . nil)) = f . fst",
>          "cup ident: cup . ((one . nil) * g) = g . snd",
>          "assocl law:  assocl . (f * (g * h)) = ((f * g) * h) . assocl",
>          "assocl help: assocl . (f * id) = ((f * id) * id) . assocl"]

> rest = ["cross bifunctor: (f * g) . (h * k) = (f . h) * (g . k)",
>         "cross bifunctor: (id * id) = id",
>         "defn cp:     cp . nil = one . nil",
>         "defn cp:  cp . cons = map cons . cpp . (id * cp)",
>         "defn unifyAll: unifyAll . nil = one . nil",
>         "defn unifyAll: unifyAll . cons = cup . (one * unifyAll)",
>         "unify after nil:  unify . (id * nil) = one . fst"]
 
> laws7 = defns ++ cmaps ++ maps ++ cups ++ rest


*Main> simplify laws7 "xmatchesA . (id * nil)"

  xmatchesA . (id * nil)
=   {defn xmatchesA}
  cup . (one * matchesA) . (id * nil)
=   {cross bifunctor}
  cup . (one * (matchesA . nil))
=   {defn matchesA}
  cup . (one * (combine . map matchA . nil))
=   {map after nil}
  cup . (one * (combine . nil))
=   {defn combine}
  cup . (one * (cmap unifyAll . cp . nil))
=   {defn cp}
  cup . (one * (cmap unifyAll . one . nil))
=   {cmap after one}
  cup . (one * (unifyAll . nil))
=   {defn unifyAll}
  cup . (one * (one . nil))
=   {cup ident}
  one . fst
 
> defns2 = ["defn match:      match = cmap matchesA . alignments",
>           --- "defn matchesA:   matchesA = combine . map matchA",
>           "defn xmatch:     xmatch    = cup . (one * match)",
>           "defn xmatchesA:  xmatchesA = cup . (one * matchesA)",
>           --- "defn xmatchA:    xmatchA   = cup . (one * matchA)",
>           "defn combine:    combine = cmap unifyAll . cp"]

> cups2  = [--- "defn cup:  cup = cmap unify . cpp",
>           "cup assoc: cup . (id * cup) = cup . (cup * id) . assocl",
>           "cup ident: cup . (f * (one . nil)) = f . fst",
>           "cup ident: cup . ((one . nil) * g) = g . snd",
>           "assocl law:  assocl . (f * (g * h)) = ((f * g) * h) . assocl",
>           "assocl help: assocl . (f * id) = ((f * id) * id) . assocl"]
> laws8 = defns2 ++ cmaps ++ maps ++ cups2 ++ rest

*Main> simplify laws8 "cmap xmatchesA . cpp . (xmatchA * one) . assocl"

  cmap xmatchesA . cpp . (xmatchA * one) . assocl
=   {defn xmatchesA}
  cmap (cup . (one * matchesA)) . cpp . (xmatchA * one) . assocl
=   {cmap-cup law}
  cup . (id * cmap matchesA) . (xmatchA * one) . assocl
=   {cross bifunctor}
  cup . (xmatchA * (cmap matchesA . one)) . assocl
=   {cmap after one}
  cup . (xmatchA * matchesA) . assocl

*Main> prove laws7 "xmatchesA . (id * cons) = cup . (xmatchA * matchesA) . assocl"

  xmatchesA . (id * cons)
=   {defn xmatchesA}
  cup . (one * matchesA) . (id * cons)
=   {cross bifunctor}
  cup . (one * (matchesA . cons))
=   {defn matchesA}
  cup . (one * (combine . map matchA . cons))
=   {map after cons}
  cup . (one * (combine . cons . (matchA * map matchA)))
=   {defn combine}
  cup . (one * (cmap unifyAll . cp . cons . (matchA * map matchA)))
=   {defn cp}
  cup . (one * (cmap unifyAll . map cons . cpp . (id * cp) . (matchA * map matchA)))
=   {cmap after map}
  cup . (one * (cmap (unifyAll . cons) . cpp . (id * cp) . (matchA * map matchA)))
=   {cross bifunctor}
  cup . (one * (cmap (unifyAll . cons) . cpp . (matchA * (cp . map matchA))))

=   {defn unifyAll}
  cup . (one * (cmap (cup . (one * unifyAll)) . cpp . (matchA * (cp . map matchA))))
=   {cmap-cup law}
  cup . (one * (cup . (id * cmap unifyAll) . (matchA * (cp . map matchA))))
=   {cross bifunctor}
  cup . (one * (cup . (matchA * (cmap unifyAll . cp . map matchA))))
=   {... ??? ...}
  cup . ((cup . (one * matchA)) * (cmap unifyAll . cp . map matchA)) . assocl
=   {defn combine}
  cup . ((cup . (one * matchA)) * (combine . map matchA)) . assocl
=   {defn xmatchA}
  cup . (xmatchA * (combine . map matchA)) . assocl
=   {defn matchesA}
  cup . (xmatchA * matchesA) . assocl

> defns3 = ["defn match:      match = cmap matchesA . alignments",
>           "defn matches:    matches = combine . map match",
>           "defn xmatch:     xmatch    = cup . (one * match)",
>           "defn xmatches:   xmatches = cup . (one * matches)",
>           "defn xmatchA:    xmatchA   = cup . (one * matchA)",
>           "defn combine:    combine = cmap unifyAll . cp"]
> laws9 = defns3 ++ cmaps ++ maps ++ cups2 ++ rest

> defnsx = ["defn cmap:   cmap f = concat . map f",
>           "map functor: map f . map g = map (f.g)",
>           "concat law:  map f . concat = concat . map (map f)",
>           "concat after concat: concat . concat = concat . map concat"]

> lawsA = ["cmap after cmap:  cmap f . map g = cmap (f . g)",
>          "cmap after cpp:  cmap cpp . cpp = cpp . (concat * concat)",
>          "cross bifunctor: (f * g) . (h * k) = (f . h) * (g . k)",
>          "map after cpp:   map (f * g) . cpp = cpp . (map f * map g)",
>          "defn cmap:       cmap f = concat . map f",
>          "concat after id: concat . map one = id"]       

> thmA = "cmap (cpp . (one * f)) . cpp = cpp . (id * cmap f)"


  cmap (cpp . (one * g)) . cpp
=   {cmap after cmap (backwards)}
  cmap cpp . map (one * g) . cpp
=   {map after cpp}
  cmap cpp . cpp . (map one * map g)
=   {cmap after cpp}
  cpp . (concat * concat) . (map one * map g)
=   {cross bifunctor}
  cpp . ((concat . map one) * concat (map g))
=   {defn cmap (backwards)}
  cpp . ((concat . map one) * cmap g)
=   {concat after id}
  cpp . (id * cmap g)
  
