module Replicate where
import Control.Applicative ( Applicative(liftA2) )

for :: Int -> (a -> a) -> a -> a
for n f m
    | n <= 0    = m
    | otherwise = for (n - 1) f (f m)


replM :: Applicative m => Int -> m a -> m [a]
replM n x = for n go (pure mempty)
              where go m = liftA2 mappend m (fmap pure x)