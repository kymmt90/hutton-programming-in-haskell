data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Foldable Tree where
  fold (Leaf x) = x
  fold (Node l r) = fold l `mappend` fold r

  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

  foldr f v (Leaf x) = f x v
  foldr f v (Node x) = foldr f (foldr f v r) l

  foldl f v (Leaf x) = f v x
  foldl f v (Node x) = foldr f (foldr f v l) r
