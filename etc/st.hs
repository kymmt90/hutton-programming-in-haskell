-- state transformer
type State = Int
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  pure x = S (\s -> (x,s))
  stf <*> stx = S (\s -> let (f,s') = app stf s
                             (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

fresh :: ST Int
fresh = S (\n -> (n, n+1))

-- label by applicative
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- label by monad
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
