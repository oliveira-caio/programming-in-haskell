import System.IO

{- Exercicio 1 -}
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

{- Exercicio 2 -}
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [] = return ()
putBoard (x:xs) = do putRow (1 + length xs) x
                     putBoard xs

putBoard1 = putBoard2 1

putBoard2 r [] = return ()
putBoard2 r (n:ns) = do putRow r n
                        putBoard2 (r+1) ns

putBoard3 :: Board -> IO ()
putBoard3 xs = sequence_ [putRow r x | (r,x) <- zip [1..] xs]

{- Exercicio 5 -}
adder :: IO ()
adder = do putStr "How many?"
           n <- getChar
           k <- show "n"
           addList <- sequence [getChar | _ <- [1..k]]
           addList2 <- [show "x" | x <- addList]
           putStr $ "The total is " ++ show (sum addList2) ++ "\n"


{- Exercicio 6 -}
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

{-readLine :: IO String
readLine = readLine' ""

readLine' :: [Char] -> IO [Char]
readLine' xs = do x <- getCh
                  case x of '\n' -> return xs
                            '\DEL' -> if null xs
                                          then readLine' ""
                                          else do putStr "\b \b"
                                                  readLine' (init xs)
                            _ -> do putChar x
                                    readLine' (xs ++ [x])-}
