{- Exercicio 2 -}
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

{- Exercicio 3 -}
expon :: Int -> Int -> Int
expon _ 0 = 1
expon n m = n * expon n (m-1)


{- Exercicio 4 -}
euclid :: Int -> Int -> Int
euclid n m | n == m = n
           | n < m = euclid n (m-n)
           | otherwise = euclid m (n-m)

{- Exercicio 6 -}
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' (x:xs) = x ++ concat xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

elem' :: Eq a => a -> [a] -> Bool
elem' t x | null x      = False
          | t == head x = True
          | otherwise   = elem' t (tail x)

{- Exercicio 7 -}
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys) | x <= y = [x] ++ merge' xs (y:ys)
                     | otherwise = [y] ++ merge' (x:xs) ys

{- Exercicio 8 -}
halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge' (msort (fst (halve xs))) (msort (snd (halve xs)))

{- Exercicio 9 -}

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n x = [head x] ++ take' (n-1) (tail x)

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' (tail xs)

fat :: Integer -> Integer
fat 0 = 1
fat n = n*fat (n-1)
