import Data.Char

{- Exercise 1 -}
halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs,drop (length xs `div` 2) xs)

{- Exercise 2 -}
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (x:y:z:xs) = z

{- Exercise 3 -}
safetail1 :: [a] -> [a]
safetail1 xs = if null xs == True then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs == True = xs
              | otherwise       = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

{- Exercise 5 -}
exer5 :: Bool -> Bool -> Bool
exer5 x y = if x == False then False else
              if y == False then False else True

{- Exercise 6 -}
exer6 :: Bool -> Bool -> Bool
exer6 x y = if x == True then y else False

{- Exercise 7 -}
exer7 :: Int -> Int -> Int -> Int
exer7 = \x -> (\y -> (\z -> x*y*z))

{- Exercise 8 -}
luhnDouble :: Int -> Int
luhnDouble x = if 2*x < 10 then 2*x else 2*x - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = if (luhnDouble x + y + luhnDouble z + w) `mod` 10 == 0 then True else False
