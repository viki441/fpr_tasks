findMinValue :: [(Int -> Int)] -> Int -> Int
findMinValue fs x = minimum [f x | f <- fs]

main :: IO ()
main = do
  print(findMinValue [(\x -> x - 1), (\x -> x + 1)] 5)
