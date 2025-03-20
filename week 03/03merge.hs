merge :: [Int] -> [Int] -> [Int]
merge xs ys = [z | z <- merge' xs ys]
  where
    merge' [] ys = ys
    merge' xs [] = xs
    merge' (x:xs') (y:ys') 
      | x < y     = x : merge' xs' (y:ys')
      | otherwise = y : merge' (x:xs') ys'


main :: IO ()
main = do
    print(merge [1,2,5] [4,6,7])
