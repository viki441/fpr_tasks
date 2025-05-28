pascal :: [[Integer]]
pascal = iterate nextRow [1]
  where
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

main :: IO ()
main = do
    print(take 5 pascal)
