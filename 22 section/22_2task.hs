findPoint :: [Double] -> Int
findPoint [0, 0] = 0
findPoint [x, 0] = 0
findPoint [0, y] = 0
findPoint [x, y]
  | x > 0 && y > 0 = 1 
  | x < 0 && y > 0 = 2  
  | x < 0 && y < 0 = 3
  | x > 0 && y < 0 = 4
 
main :: IO ()
main = do
  print(findPoint [-9, 2])
