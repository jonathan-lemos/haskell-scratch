module Main where
import Control.Applicative

f n = case liftA2 rem [n] [3, 5] of
        [0, 0] -> "FizzBuzz"
        [_, 0] -> "Buzz"
        [0, _] -> "Fizz"
        [_, _] -> show n

main = mconcat $ putStrLn . f <$> [1..100]

