split :: [a] -> Int -> [[a]]
split [] _ = []
split xs n
  | length xs < n = []
  | otherwise = [x | x <- take n xs] : split (drop n xs) n


main :: IO ()
main = do 
    print(split [1,2,3,4,5,6,7]3)
