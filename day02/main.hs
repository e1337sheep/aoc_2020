import Data.List as List
import Data.Text as Text
import Parsing

-- A parser for password records
record :: Parser (Int, Int, Text)
record = do min <- int
            char '-'
            max <- int
            space
            match <- letter
            char ':'
            space
            return (min, max, pack [match])

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

charAt :: Int -> Text -> Text
charAt i inp = Text.take 1 $ Text.drop (i-1) inp

policy1 :: ((Int, Int, Text), String) -> Bool
policy1 ((min, max, match), inp) = inRange min max $ Text.count match $ pack inp
  where inRange min max n = min <= n && max >= n

policy2 :: ((Int, Int, Text), String) -> Bool
policy2 ((min, max, match), inp) = (c min == match) `xor` (c max == match)
  where c n = charAt n $ pack inp

day02 :: (((Int, Int, Text), String) -> Bool) -> FilePath -> IO ()
day02 pf txt = do
  contents <- readFile txt
  let list = List.concatMap (parse record) $ List.lines contents
  let good = List.filter pf list
  print $ List.length good