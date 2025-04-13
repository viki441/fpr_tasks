applyNTimes :: (Int -> Int) -> Int -> (Int -> Int)
applyNTimes f 0 = \x -> x 
applyNTimes f n = \x -> f (applyNTimes f (n - 1) x)

double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
  print((applyNTimes double 3) 2)
