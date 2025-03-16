isLeapYear :: Int -> Bool
isLeapYear x
  | x `mod` 2 /= 0 = False
  | x `mod` 400 == 0 = True
  | x `mod` 100 == 0 && x `mod` 400 /= 0 = False
  | x `mod` 4 == 0 && x `mod` 100 /= 0 = True
  |otherwise = False

main :: IO ()
main = do
  print(isLeapYear 2022)
