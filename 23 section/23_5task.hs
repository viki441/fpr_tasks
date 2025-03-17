commonEl :: Eq a => [a] -> [a] -> Int
commonEl [] _ = 0
commonEl l1 l2
  | head l1 `elem` l2 = 1 + commonEl (tail l1) l2
  | otherwise = commonEl (tail l1) l2

main :: IO ()
main = do
  print(commonEl [2,3,4,5,6] [1,2,3,4,5,6])
