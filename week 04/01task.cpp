removeConsecutive :: Eq a => [a] -> [a]
removeConsecutive xs = [x | (x, keep) <- zip xs (True : zipWith (/=) xs (tail xs)), keep]

main :: IO ()
main = do
  print(removeConsecutive [1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,9])
