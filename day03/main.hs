rotate n xs = take len $ drop (len + n) $ cycle xs
  where len = length xs

skate :: (Int, Int) -> Int -> [String] -> Int
skate (x, y) trees course = if null course' then trees else skate (x, y) (trees + isTree) course'
  where course' = map (rotate x) $ drop y course
        isTree = fromEnum $ '#' == head (head course')

vs1 :: [(Int, Int)]
vs1 = [(3,1)]
vs2 :: [(Int, Int)]
vs2 = [ (1,1), (3,1), (5,1), (7,1), (1,2) ]

day03 :: [(Int, Int)] -> FilePath -> IO ()
day03 vs txt = do
  contents <- readFile txt
  let run v = skate v 0 $ lines contents
  print $ product $ map run vs