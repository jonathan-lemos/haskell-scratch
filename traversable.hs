import Control.Applicative

data Optional a =
      Some a
    | None
    deriving Show

instance Functor Optional where
    fmap f None = None
    fmap f (Some s) = Some (f s)

instance Applicative Optional where
    pure = Some
    None <*> _ = None
    _ <*> None = None
    Some f <*> Some g = Some (f g)

instance Foldable Optional where
    foldr f i None = i
    foldr f i (Some s) = f s i

instance Traversable Optional where
    sequenceA (Some f) = Some <$> f
    sequenceA None = pure None


data List a = Nil | Cons a (List a) deriving Show

instance Semigroup (List a) where
    Nil <> Nil = Nil
    Nil <> Cons head tail = Cons head tail
    Cons ah at <> b = Cons ah (at <> b)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons head tail) = Cons (f head) (fmap f tail)

instance Applicative List where
    pure s = Cons s Nil
    Nil <*> _ = Nil
    Cons head tail <*> b =
        (head <$> b) <> (tail <*> b)

instance Foldable List where
    foldr f i Nil = i
    foldr f i (Cons head tail) = f head (foldr f i tail)

instance Traversable List where
    sequenceA Nil = pure Nil
    sequenceA (Cons head tail) = liftA2 Cons head (sequenceA tail)