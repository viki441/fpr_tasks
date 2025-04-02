--task A
removeX :: Eq a => a -> [a] -> [a]
removeX _ [] = []
removeX x (y:ys) 
    | x == y = drop 1 (y:ys)
    | otherwise =  y : removeX x ys

--task B    
isElement :: Eq a => a -> a -> Bool
isElement a b = if a == b then False else True

removeAllX :: Eq a => a -> [a] -> [a]
removeAllX _ [] = []
removeAllX x list = [a | a <- list,  isElement x a]

main :: IO ()
main = do
    print (removeAllX 3 [1,1,1,2,3,3,3,4,4,4,5,5,7])
