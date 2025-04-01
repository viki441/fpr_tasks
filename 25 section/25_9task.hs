--solution 1
flattenL :: [[a]] -> [a]
flattenL [] = []
flattenL (x:xs) = x ++ flattenL xs

--solution 2
flattenL :: [[a]] -> [a]
flattenL = concat 

main :: IO ()  
main = do
    print (flattenL [[1, 3, 2], [2, 4, 6, 1]])  
