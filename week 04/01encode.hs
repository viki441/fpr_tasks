encode :: String -> [(Char, Int)]
encode [] = []
encode (x:xs) = (x, count x (x:xs)) : encode (remove x (x:xs))
  where

    count :: Eq a => a -> [a] -> Int
    --first filter all == and then get length
    count x = length . filter (== x)

    remove :: Eq a => a -> [a] -> [a]
    remove _ [] = []
    remove x (y:ys)
      | x == y = remove x ys
      | otherwise = y : remove x ys

main :: IO ()
main = do
  print(encode "adsasdsdasd")
