squareAll :: [Double] -> [Double]
squareAll list = map (^2) list

main :: IO ()
main = do
  print(squareAll [1,2,3,4])
