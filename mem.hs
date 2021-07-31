module Mem where

newtype Mem s a =
    Mem {
        runMem :: s -> (a, s)
    }

instance Monoid a => Semigroup (Mem s a) where
    (<>) a b = Mem $ \s ->
                 let (aa, sa) = runMem a s
                     (ab, sb) = runMem b sa
                     na = aa <> ab
                     in (na, sb)

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend a b = a <> b

f' = Mem $ \s -> ("hi", s + 1)

func = do
    let rmzero = runMem mempty (0 :: Int)
        rmleft = runMem (f' <> mempty) (0 :: Int)
        rmright = runMem (mempty <> f') (0 :: Int)
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' (0 :: Int)
    print $ rmright == runMem f' (0 :: Int)
