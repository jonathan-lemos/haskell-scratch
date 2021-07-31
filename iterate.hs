module Iter where

iter :: (a -> a) -> a -> [a]
iter f i = i : iter f (f i)

unfoldRight :: (b -> Maybe (a, b)) -> b -> [a]
unfoldRight func initial =
    case func initial of
        Just (value, next) -> value : unfoldRight func next
        Nothing -> []

testFunc 0 = Nothing 
testFunc i = Just (100 + i, i - 1)