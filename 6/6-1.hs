fact :: Int -> Int
fact 0 = 1
fact x | x < 0 = 1
       | otherwise = x * fact (x - 1)
