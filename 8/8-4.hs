data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

halve :: [a] -> ([a], [a])
halve xs = splitAt halfLength xs
  where
    halfLength = (length xs) `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs =
    Node (balance fstHalved) (balance sndHalved)
  where
    (fstHalved, sndHalved) = halve xs
