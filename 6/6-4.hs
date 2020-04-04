euclid :: Int -> Int -> Int
euclid x y | y == 0 = x
           | x > y = euclid y (x `mod` y)
           | x < y = euclid x (y `mod` x)
