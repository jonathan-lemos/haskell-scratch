module Compose where

import Data.Monoid

f :: Comp String
f = Comp $ \a -> concat ["f(", a, ")"]

g :: Comp String
g = Comp $ \a -> concat ["g(", a, ")"]

newtype Comp a =
    Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
    (<>) a b = Comp $ unComp a <> unComp b

instance Semigroup a => Monoid (Comp a) where
    mempty = Comp id
    mappend a b = a <> b

