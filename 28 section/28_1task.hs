--filter
filterFunc :: (a -> Bool) -> [a] -> [a]
filterFunc _ [] = []
filterFunc f (x:xs)
    | f x = x : filterFunc f xs
    | otherwise = filterFunc f xs

--map
mapFunc :: (a -> b) -> [a] -> [b]
mapFunc _ [] = []
mapFunc f (x:xs)  = f x : mapFunc f xs

--foldl
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

--foldr
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

--foldl1
myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [] = error "empty:l"
myFoldl1 f (x:xs) =  myFoldl f x xs

--foldr1
myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [] = error "empty:r"
myFoldr1 _ [x] = x
myFoldr1 f (x:xs) = f x (myFoldr1 f xs)

main :: IO ()
main = do
    print(mapFunc (*2) [1,2,3])
    print(filterFunc even [1,2,3])
    print(myFoldl (+) 0 [1,2,3])
    print(myFoldr (-) 0 [1,2,3])  --1 - (2 - (3 - 0)) = 2
    print(myFoldl1 (-) [1,2,3])
    print(myFoldr1 (+) [1,2,3])
