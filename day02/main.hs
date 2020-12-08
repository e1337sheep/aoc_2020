import Data.List as List
import Data.Text as Text
import Parsing

record = do min <- int
            char '-'
            max <- int
            space
            match <- letter
            char ':'
            space
            return (min, max, pack [match])

inRange min max n = min <= n && max >= n

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

charAt :: Int -> Text -> Text
charAt i inp = Text.take 1 $ Text.drop (i-1) inp

policy2 :: ((Int, Int, Text), String) -> Bool
policy2 ((min, max, match), inp) = (charAt min pinp == match) `xor` (charAt max pinp == match)
  where pinp = pack inp

-- comply :: (Int, Int, Text) -> String -> Int
policy1 :: ((Int, Int, Text), String) -> Bool
policy1 ((min, max, match), inp) = inRange min max $ Text.count match $ pack inp

day02 pf txt = do
  contents <- readFile txt
  let list = List.concatMap (parse record) $ List.lines contents
  let good = List.filter pf list
  -- print list
  -- print good
  print $ List.length good
  print "============="
  print "END OF OUTPUT"