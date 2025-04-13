split :: (a -> Bool) -> [a] -> ([a], [a])
--means: "first apply p, then apply not to the result"
--because "not" without anything is for bool
split p xs = (filter p xs, filter (not . p) xs)

main :: IO ()
main = do
  print(split odd [2,4,3,4,2,1,2,3])
