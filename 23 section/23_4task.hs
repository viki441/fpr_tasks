isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True  
isSublist _ [] = False   
isSublist l1 l2@(x:xs)
  | startsWith l1 l2 = True
  | otherwise = isSublist l1 xs


startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (y:ys) (z:zs)
  | y == z = startsWith ys zs
  | otherwise = False
