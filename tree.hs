import Control.Applicative

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Branch left element right) = inorder left ++ [element] ++ inorder right

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Branch left element right) = Branch (fmap f left) (f element) (fmap f right)

instance Applicative Tree where
    pure a = Branch Leaf a Leaf
    Leaf <*> b = Leaf
    a <*> Leaf = Leaf
    (Branch al ae ar) <*> (Branch bl be br) = Branch (al <*> bl) (ae be) (ar <*> br)

instance Foldable Tree where
    foldr f i Leaf = i
    foldr f i (Branch left element right) = f element (foldr f (foldr f i left) right)

instance Traversable Tree where
    sequenceA Leaf = pure Leaf
    sequenceA (Branch left element right) = liftA3 Branch (sequenceA left) element (sequenceA right)