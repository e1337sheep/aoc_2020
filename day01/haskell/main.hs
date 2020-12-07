
import Data.List

-- pairs :: [a] -> [(a, a)]
-- pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

pairs :: [a] -> [[a]]
pairs l = [[x, y] | (x:ys) <- tails l, y <- ys]

triplets :: [a] -> [[a]]
triplets l = [[x, y, z] | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs ]

-- sumis :: (Eq a, Num a) => a -> (a, a) -> Bool
-- sumis x (y1, y2) = y1 + y2  == x

sumis t x = sum x == t 

-- sumis2020 = sumis 2020

sumis2020 :: (Eq a, Num a) => [a] -> Bool
sumis2020 = sumis 2020


answer (x1, x2) = x1 * x2

answer2 :: Num a => [a] -> a
answer2 [x] = x
answer2 (x:xs) = x * answer2 xs

blastStr = putStr
readInt x = read x :: Int

-- day01a :: String -> IO ()
-- day01a x = do
--     contents <- readFile x
--     let rs = map readInt $ lines contents
--     let correct = map answer $ filter sumis2020 (pairs rs)
--     print correct

day01a' :: String -> IO ()
day01a' x = do
    contents <- readFile x
    let rs = map readInt $ lines contents
    let correct = map product $ filter sumis2020 (pairs rs)
    print correct
    print $ map product $ filter sumis2020 $ pairs $ map readInt $ lines contents

day01 :: ([a] -> [[a]]) -> IO ()
day01 f x = do
    contents <- readFile x
    print $ maps product $ filter sumis2020 $ f $ map readInt $ lines contents
-- day01b x = do
--     contents <- readFile x
--     let rs = map readInt $ lines contents
--     let correct = map answer2 $ filter sumis2020' (triplets rs)
--     print correct



