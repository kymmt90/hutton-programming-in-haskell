data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

tree :: Tree Int
tree = Node (Node Leaf 1 (Node Leaf (-3) Leaf)) 0 Leaf
