dotProd :: [Double] -> [Double] -> Double
dotProd v1 v2 = sum (zipWith (*) v1 v2)

main :: IO ()
main = do
  print(dotProd [1,2,3] [4,5,6])
