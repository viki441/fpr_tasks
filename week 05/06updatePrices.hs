updatePrices :: Double -> ([Double] -> [Double])
updatePrices x = map (\(i, p) -> if even i 
                                    then p * (1 - x / 100) 
                                    else p * (1 + x / 100)) . zip [0..]
main :: IO ()
main = do
  print  ((updatePrices 4) [1, 6.4 , 2, 6,3, 3, 4])
