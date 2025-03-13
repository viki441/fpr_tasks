taskM :: Int -> Bool
taskM x = isSequence (digits x)
--fisrt turn it to list
digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]
-- check for sequence
isSequence :: [Int] -> Bool
isSequence [] = True 
isSequence [_] = True
isSequence (a:b:rest) = all (== d) differences
  where
    d = b - a 
    differences = zipWith (-) (b:rest) (a:b:rest)
