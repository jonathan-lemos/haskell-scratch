module Foldable where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' a = getSum $ foldMap Sum a

product' :: (Foldable t, Num a) => t a -> a
product' a = getProduct $ foldMap Product a

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr (\c a -> a || e == c) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
                where f c a = Just (case a of
                                Nothing -> c
                                Just aa -> if aa < c then aa else c)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (max . Just) Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr ((const . const) True) False

length' :: Foldable t => t a -> Int
length' = foldr (flip (const . (+1))) 0

length'' :: Foldable t => t a -> Int
length'' = getSum . foldMap (const (Sum 1))

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\c a -> f c <> a) mempty

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldr go mempty
                where go c a =
                        if f c then a <> pure c else a