newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  -- (Z gs) <*> (Z xs) = Z (let gxs = zip gs xs
  --                            h [] = []
  --                            h (y:ys) = ((fst y) (snd y)) : h ys
  --                        in h gxs)
  (Z gs) <*> (Z xs) = Z ([g x | (g, x) <- zip gs xs])
