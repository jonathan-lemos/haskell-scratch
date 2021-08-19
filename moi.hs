module Moi where

import Control.Applicative

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi m) = let mapTuple (a, b) = (f a, b) in
                        Moi (mapTuple . m)

instance Applicative (Moi s) where
    pure a = Moi $ (,) a
    -- a  :: s -> (a, s)
    -- ab :: s -> (a -> b, s)
    (Moi ab) <*> (Moi a) =
        let transform (a, s) =
                let (abf, abs) = ab s in
                    (abf a, abs) in
            Moi (transform . a)

instance Monad (Moi s) where
    -- f :: s -> (a, s)
    -- g :: a -> Moi { runMoi :: s -> (b, s) }
    (Moi f) >>= g =
        let transform (a, s) =
                let (Moi mb) = g a in
                    mb s in
            Moi (transform . f)