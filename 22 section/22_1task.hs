triangleAreaList :: [Double] -> Double
triangleAreaList [base, height] = (base * height) / 2 
triangleAreaList [a, b, c] = sqrt (s * (s - a) * (s - b) * (s - c)) 
  where
    s = (a + b + c) / 2
main :: IO ()
main = do
  print(triangleAreaList [2.5, 4])
  print(triangleAreaList [2.0, 4.0, 6.0])
