calcAvgGrade :: [(String, Double)] -> Double
calcAvgGrade list = average [ b | (a , b)<- list]

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
  print  (calcAvgGrade  [("ivan", 6), ("gosho", 4)])
