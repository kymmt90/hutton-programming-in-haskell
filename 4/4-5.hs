(.&&.) :: Bool -> Bool -> Bool
(.&&.) l r = if l == True then
               if r == True then True else False
             else False
