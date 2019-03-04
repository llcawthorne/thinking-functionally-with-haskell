Common words.
10th October, 2014
In Chapter 01.
_________________________________________

> import Data.Char (toLower)
> import Data.List (sort,words)
> import System.IO

> type Word = [Char]
> type Code = (Int,Word)

> sortWords :: [Word] -> [Word]
> sortWords = sort

> codeRuns :: [Word] -> [Code]
> codeRuns []     = []
> codeRuns (w:ws) = (1+length us,w):codeRuns vs
>                    where (us,vs) = span (==w) ws

> sortCodes :: [Code] -> [Code]
> sortCodes = reverse . sort

> showCode :: Code -> [Char]
> showCode (n,w) = w ++": " ++ show n ++ "\n"

> commonWords :: Int -> [Char] -> [Char]
> commonWords n = concat . map showCode . take n . sortCodes .
>                 codeRuns . sortWords . words . map toLower 


> cwords :: Int -> FilePath -> FilePath -> IO()
> cwords n ifile ofile
>      = do {text <- readFile ifile;
>            writeFile ofile (commonWords n text);
>            putStrLn "cwords done!"}

> main :: IO ()
> main 
>   = do {hSetBuffering stdout NoBuffering; --- don't ask!
>         putStrLn "Take text from where: ";
>         ifile <- getLine;
>         putStrLn "How many words: ";
>         n <- getLine;
>         putStrLn "Put results where: ";
>         ofile <- getLine;
>         text <- readFile ifile;
>         writeFile ofile (commonWords (read n) text);
>         putStrLn "cwords done!" }

