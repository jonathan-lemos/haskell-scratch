module Main where

fix :: (a -> a) -> a
fix f = let {x = f x} in x

main :: IO ()
main = print $ fix (1:)