halve :: [a] -> ([a], [a])
halve xs = (take halfLength xs, drop halfLength xs)
             where
               halfLength = (length xs) `div` 2
