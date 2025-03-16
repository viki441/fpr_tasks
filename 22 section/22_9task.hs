countNumbersInFormula :: Int -> Int 
countNumbersInFormula n = solveFormula n 1 0
solveFormula :: Int -> Int -> Int -> Int
solveFormula 0 0 _ = 0
solveFormula n i sum
  | i > n = sum
  | s `mod` 5 == 0 || s `mod` 9 == 0 = solveFormula n (i + 1) (sum + 1)
  |otherwise = solveFormula n (i + 1) sum
  where
    s = i ^ 3 + 13 * i * n + n ^ 3

main :: IO ()
main = do
  print(countNumbersInFormula 2)
