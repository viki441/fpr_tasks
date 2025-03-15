import Prelude
taskA :: Int -> Bool
taskA x = if x `mod` 4 == 0|| x `mod` 7 == 0 then True else False

taskB :: [Int] -> Bool
taskB [0, b, c] = False
taskB [a, b, c] = if  (b ^ 2 - 4 * a * c) < 0 then True else False

taskC :: [Double]-> [Double] -> Double -> Bool
--excluding border
taskC [a, b] [x, y] r = (a - x) ^ 2 + (b - y) ^ 2 < r ^ 2

--and check if in 3rd quadrant
taskD :: [Double] -> [Double] -> Double -> Bool
taskD [a, b] [x, y] r = a < 0 && b < 0 && taskC [a, b][x, y] r

taskE :: [Double] -> Double -> Double -> Bool
--centre (0, 0)
taskE [a, b] r1 r2 =  a ^ 2 + b ^ 2 >= r1 ^ 2 && a ^ 2 + b ^ 2 <= r2 ^ 2

taskF :: [Double] -> Bool
taskF [x, y] = x == 0 && y >= 0 && y <= 1

taskG :: Int -> [Int] -> Bool
taskG x [a,b,c]
  | x == max (max a b) c = True
  |otherwise = False

taskH :: Int -> [Int] -> Bool
taskH x [a,b,c] = not (taskG x [a,b,c])

--all elements are negative
taskI :: [Int] -> Bool
taskI xs = all (< 0) xs



taskJ :: Int -> Int -> Bool
taskJ x 0 = False
taskJ x y
  | y `mod` 10 == x = True
  |otherwise = taskJ x (y `div` 10)





taskK :: Int -> Bool
taskK x = checkDigits x []
checkDigits :: Int -> [Int] -> Bool
checkDigits 0 _ = True
checkDigits x seen
  | lastDigit `elem` seen = False  
  --(lastDigit : seen) - puts the last non repeating digit in the array
  | otherwise = checkDigits (x `div` 10) (lastDigit : seen)
  where 
    lastDigit = x `mod` 10




taskL :: Int -> Bool
taskL x = countDigits x [] 0
countDigits :: Int -> [Int] -> Int -> Bool
countDigits 0 _ s = s >= 2 --that would return false
countDigits x el s
  | lastN `elem` el = countDigits (x `div` 10) el (s + 1) 
  | otherwise = countDigits (x `div` 10) (lastN : el) s 
  where
    lastN = x `mod` 10






taskM :: Int -> Bool
taskM x = isSequence (digits x)
--fisrt turn it to list
digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]
-- check for sequence
isSequence :: [Int] -> Bool
isSequence [] = True 
isSequence [_] = True
isSequence (a:b:rest) 
  | a > b = isBigger (b:rest)
  | a < b = isSmaller (b: rest)
  | otherwise = False

isBigger :: [Int] -> Bool
isBigger [] = True
isBigger [_] = True
isBigger (a:b:rest)
  |a < b = isBigger (b:rest)
  |otherwise = False

isSmaller :: [Int] -> Bool
isSmaller [] = True
isSmaller [_] = True
isSmaller (a:b:rest)
  | a > b = isSmaller (b:rest)
  |otherwise = False



taskN :: Double -> Double
taskN x = fromIntegral (round ((x - fromIntegral (floor x)) * 100)) / 100


taskO :: Int -> Bool
taskO x
  | x < 2 = False 
  | otherwise = isPrime x 2

isPrime :: Int -> Int -> Bool
isPrime x divisor
  | divisor * divisor > x = True
  | x `mod` divisor == 0 = False 
  | otherwise = isPrime x (divisor + 1)  




main :: IO ()
main = do
  putStrLn "Task a:"
  print(taskA 9)
  putStrLn "Task b:"
  print(taskB [0,4,4])
  putStrLn "Task c:"
  print(taskC [1,2] [0,0] 5)
  putStrLn "Task d:"
  print(taskD [1,2] [5,5] 8)
  putStrLn "Task e:"
  print(taskE [1,2] 5 10)
  putStrLn "Task f"
  print(taskF [0,0.5])
  putStrLn "Task g"
  print(taskG 3 [1,2,6])
  putStrLn "Task h"
  print(taskH 3 [1,2,6])
  putStrLn "Task I"
  print (taskI [-1,2,-3]) 
  putStrLn "Task J"
  print(taskJ 7 21378)
  putStrLn "Task K"
  print(taskK 1223)
  putStrLn "Task L"
  print(taskL 12223)
  putStrLn "Task M"
  print(taskM 12343)
  putStrLn "Task N"
  print(taskN 2.88)
  putStrLn "Task O"
  print(taskO 3)
