halve :: [a] -> ([a], [a])
halve xs =
    (take halfLength xs, drop halfLength xs)
  where
    halfLength = (length xs) `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y  = y : (merge (x:xs) ys)
  | x <= y = x : (merge (y:ys) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort fstHalved) (msort sndHalved)
  where
    halved = halve xs
    fstHalved = fst halved
    sndHalved = snd halved
