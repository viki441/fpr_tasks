merge :: [Int] -> [Int] -> [Int]
merge list1 list2 = [z | z <- merge' list1 list2]
  where
    merge' [] list2 = list2
    merge' list1 [] = list1
    merge' (x:list1') (y:list2') 
      | x < y     = x : merge' list1' (y:list2')
      | otherwise = y : merge' (x:list1') list2'


main :: IO ()
main = do
    print(merge [1,2,5] [4,6,7])
