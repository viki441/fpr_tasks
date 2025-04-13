isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a

triags :: [Double] -> [Double] -> [Double] -> [(Double, Double, Double)]
--triags xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs, isTriangle x y z]
triags xs ys zs = [(x, y, z) | (x, (y, z)) <- zip xs (zip ys zs), isTriangle x y z]
main :: IO ()
main = do
  print(triags [1,2,3] [4,3,2] [3,4,2])
