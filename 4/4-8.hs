luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0
