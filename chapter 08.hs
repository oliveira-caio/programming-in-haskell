{- Exercise 1 -}
data Nat = Zero | Succ Nat
         deriving (Show, Read)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ Zero) n = n
mult (Succ m) n = add (mult m n) n





{- Exercise 2 -}
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
    (Node (Leaf 6) 7 (Leaf 9))





{- Exercise 3 -}
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)
             deriving (Show, Read)             

leaves :: Tree2 a -> Int
leaves (Leaf2 y) = 1
leaves (Node2 l r) = (leaves l) + (leaves r)

t2 :: Tree2 Int
t2 = Node2 (Node2 (Leaf2 1) (Leaf2 (3))) (Node2 (Leaf2 6) (Leaf2 9))

t3 :: Tree2 Int
t3 = Node2 (Leaf2 1) (Node2 (Node2 (Leaf2 3) (Leaf2 5)) (Leaf2 4))

balanced :: Tree2 a -> Bool
balanced (Leaf2 x) = True
balanced (Node2 l r) = balanced l && balanced r
                     && abs (leaves l - leaves r) <= 1





{- Exercise 4 -}
halve :: [a] -> [[a]]
halve [] = []
halve xs = [take (length xs `div` 2) xs] ++ [drop (length xs `div` 2) xs]

balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs = Node2 (balance (head (halve xs))) (balance (head (reverse (halve xs))))
