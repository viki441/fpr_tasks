removeAllDuplicates :: Eq a => [a] -> [a]
removeAllDuplicates list = [ x | (x, i) <- zip list [0..], notElem x (take i list) ]

main :: IO ()
main = do
    print (removeAllDuplicates [1,1,1,2,3,3,3,4,4,4,5,5,7])
