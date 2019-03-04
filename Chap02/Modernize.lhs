Modernize
24th November, 2013
In Chapter 02
_________________________________________________

> import Data.List (words, unwords)
> import Data.Char (toUpper)

> type Word = [Char]

> modernize :: String -> String
> modernize = unwords . map capitalize . words

> capitalize :: Word -> Word
> capitalize xs = [toUpper (head xs)] ++ tail xs
