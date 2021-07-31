module Main where

import Data.List (intersperse)

(|>) :: (t1 -> t2) -> (t2 -> t3) -> t1 -> t3
(f |> g) x = g (f x)

digitToWord :: Integral a => a -> String
digitToWord digit =
    case digit of
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"

digits :: Integral a => a -> [a]
digits 0 = [0]
digits n =
    go n []
    where go remainder sofar
            | remainder == 0 = sofar
            | otherwise = go d (r : sofar)
                where (d, r) = remainder `divMod` 10

join :: String -> [String] -> String 
join sep list =
    go sep list ""
    where go sep list str =
              case list of
                  [] -> str
                  [x] -> str ++ x
                  (x:xs) -> go sep xs (str ++ x ++ sep)

wordNumber :: Integral a => a -> String 
wordNumber = digits |> map digitToWord |> join " "

main :: IO ()
main = print $ wordNumber 123