calcRiemanSum :: Double -> (Double -> Double) -> (Double, Double) -> Double
calcRiemanSum delta f (a, b) = sum [f (a + i * delta) * delta | i <- [0..(n-1)]]
  where
    n = round ((b - a) / delta)

func :: (Double, Double) -> Double
func = calcRiemanSum 0.01 (\x -> x*x)

main :: IO ()
main = do
    print (func (0, 5))  
