Literate Haskell Scripts to Haskell scripts
13th October, 2012
In Chapter 09

> import Data.List (lines)

> main = interact (unlines . map cleanup . filter code . lines)
> code xs = null xs || head xs == '>'
> cleanup xs = if null xs then [] else tail xs
