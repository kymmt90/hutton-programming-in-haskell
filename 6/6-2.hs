sumdown :: Int -> Int
sumdown 0 = 0
sumdown x | x < 0 = 0
          | otherwise = x + sumdown (x - 1)
