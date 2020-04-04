import Data.Traversable

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable Tree where
  traverse f (Leaf x) = pure Leaf <*> f x
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r
