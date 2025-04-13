split :: (a -> Bool) -> [a] -> ([a], [a])
--means: "first apply p, then apply not to the result"
--because "not" without anything is for bool
split p xs = (filter p xs, filter (not . p) xs)

main :: IO ()
main = do
  print(dotProd [1,2,3] [4,5,6])
