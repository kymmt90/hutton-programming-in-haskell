data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) (Leaf 4))
         (Node (Leaf 6) (Leaf 9))

u :: Tree Int
u = Node (Node (Leaf 1) (Node (Leaf 5) (Leaf 11)))
         (Node (Leaf 6) (Leaf 9))

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node x y) = leaves x + leaves y

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) =
    abs ((leaves l) - (leaves r)) <= 1 && balanced l && balanced r
