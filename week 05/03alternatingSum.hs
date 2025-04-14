alternatingSum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
alternatingSum f g n = \x -> helper 0 x
    where
        helper i prev
            | i == n = 0
            | i `mod` 2 == 0 = f prev + helper (i+1) (f prev)
            | otherwise = g prev + helper (i+1) (g prev)

main :: IO ()
main = do
  print  ((alternatingSum (+1) (*2) 5) 2)
