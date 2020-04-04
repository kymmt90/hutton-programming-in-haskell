instance Applicative ((->) a) where
  -- pure :: a -> b -> a
  pure = const

  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  f <*> g = \x -> f x (g x)
