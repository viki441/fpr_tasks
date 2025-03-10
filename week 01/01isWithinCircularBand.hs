isWithinCircularBand :: (Double, Double) -> Double -> Double -> Bool
isWithinCircularBand (x, y) r R
    | rSquared <= distanceSquared && distanceSquared <= RSquared = True
    | otherwise = False
  where
    distanceSquared = x^2 + y^2
    rSquared = r^2
    RSquared = R^2

main :: IO ()
main = do
    print (isWithinCircularBand (3, 4) 2 6) 
    print (isWithinCircularBand (1, 1) 2 5) 
    print (isWithinCircularBand (7, 0) 2 6) 
    print (isWithinCircularBand (0, 0) 0 1) 
