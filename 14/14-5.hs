instance Monoid b => ((->) a b) where
  mempty = \a -> mempty

  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend` g = \a -> f a `mappend` g a
