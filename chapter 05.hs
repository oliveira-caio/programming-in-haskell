{- Exercise 1 -}
sumsquare :: Int -> Int
sumsquare n = sum [i*i | i <- [1..n]]

{- Exercise 2 -}
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

{- Exercise 3 -}
square :: Int -> [(Int,Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

{- Exercise 4 -}
replicate' :: Int -> a -> [a]
replicate' n x = [x | i <- [0..(n-1)]]

{- Exercise 5 -}
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

{- Exercise 6 -}
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == (sum (factors x) - x)]

{- Exercise 9 -}
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum [fst k * snd k | k <- zip x y]
