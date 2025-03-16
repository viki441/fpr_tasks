import Prelude
isPower :: Double -> Double -> Bool
isPower 0 0 = True
isPower _ 0 = False
isPower 0 _ = False
isPower x k
  | x == 1 = True
  | x / k == fromIntegral (floor (x / k)) = isPower (x / k) k
  |otherwise = False

main :: IO ()
main = do 
  print(isPower 1 2)
