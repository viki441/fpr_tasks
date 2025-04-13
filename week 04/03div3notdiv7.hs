div3notdiv7 :: [Int] -> [Int]
div3notdiv7 = filter (\x -> x `mod` 3 == 0 && x `mod` 7 /= 0)
--also works: div3notdiv7 list = filter (\x -> x `mod` 3 == 0 && x `mod` 7 /= 0) list


main :: IO ()
main = do
  print(div3notdiv7 [1,2,3,4,21,9])
