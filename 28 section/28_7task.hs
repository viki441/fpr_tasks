split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [] 
split p xs = go xs [] 


go :: (a -> Bool) -> [a] -> [a] -> [[a]]
go _ [] current = [current]
go p (y:ys) current
  | p y       = if null current then go p ys current 
                else current : go p ys [] 
  | otherwise = go p ys (current ++ [y]) 

main :: IO ()
main = do
    print (split (== ',') "part1, part 2, part3")
