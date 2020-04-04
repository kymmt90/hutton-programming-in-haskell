(.^.) :: Int -> Int -> Int
x .^. 0 | x == 0 = 0
        | otherwise = 1
x .^. y = x * (x .^. (y - 1))
