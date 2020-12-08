
import Data.List

pairs :: [a] -> [[a]]
pairs l = [[x, y] | (x:ys) <- tails l, y <- ys]

-- refactor?
triplets :: [a] -> [[a]]
triplets l = [[x, y, z] | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs ]

sumis t x = sum x == t 
sumis2020 :: (Eq a, Num a) => [a] -> Bool
sumis2020 = sumis 2020

--surely this is already a thing?
readInt x = read x :: Int

-- day01 :: ([a] -> [[a]]) -> String -> IO ()
-- day01 :: (Num b, Eq b) => ([Int] -> [[b]]) -> FilePath -> IO ()
day01 :: (Show b, Num b, Eq b) => ([Int] -> [[b]]) -> FilePath -> IO ()
day01 f x = do
    contents <- readFile x
    let rs = map readInt $ lines contents
    let answer = map product $ filter sumis2020 $ f rs
    print answer
    print "Yay"




