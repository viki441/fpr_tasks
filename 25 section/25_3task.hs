--solution 1
countOdd :: [Int] -> Int
countOdd [] = 0
countOdd (x:xs) = if x `mod` 2 == 1 then 1 + countOdd xs else countOdd xs

countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs) = if x `mod` 2 == 0 then 1 + countEven xs else countEven xs

countEvenOddl :: [Int] -> (Int, Int)
countEvenOddl xs = (countEven xs, countOdd xs)

--solution 2
countEvenOddl :: [Int] -> (Int, Int)
countEvenOddl xs = (length (filter even xs), length (filter odd xs))
