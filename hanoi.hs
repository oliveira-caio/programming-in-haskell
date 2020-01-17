hanoi :: Int -> String -> String -> String -> [(String,String)]
hanoi 0 _ _ _ = []
hanoi n x y z = hanoi (n-1) x z y ++ [(x,z)] ++ hanoi (n-1) y x z
