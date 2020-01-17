{- Exercicio 1 
map . filter -}

{- Exercicio 2 -}
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x       = [x] ++ takeWhile' p xs
                    | otherwise = takeWhile' p []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = [x] ++ xs

{- Exercicio 3 -}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

{- Exercicio 5 -}
dec2int :: [Int] -> Int
dec2int ns = foldl (\x y -> 10*x + y) 0 ns

{- foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

foldl' (\x y -> 10*x + y) 11 [2]
foldl' (\x y -> 10*x + y) (110 + 2) [] -}

{- Exercicio 5 -}
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f p = f (fst p) (snd p)

{- Exercicio 6 -}
type Bit = Int

unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map2 f = unfold null (f.head) tail

iterate2 f = unfold (\x -> False) f f

{- Exercicio 9 -}
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:y:xs) = (f x) : (g y) : altMap f g xs

{- Exercicio 10 -}
luhnDouble :: Int -> Int
luhnDouble x = if 2*x < 10 then 2*x else 2*x - 9

luhn :: [Int] -> Bool
luhn xs = if ((sum . altMap luhnDouble id) xs `mod` 10) == 0 then True else False
