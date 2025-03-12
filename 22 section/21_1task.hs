triangleArea :: Double -> Double -> Double
triangleArea side h = (side * h) / 2

triangleArea2 :: Double -> Double -> Double -> Double
triangleArea2 a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2 

main :: IO ()
main = do
  print(triangleArea 2.5 4)
  print(triangleArea2 2.0 4.0 6.0)
