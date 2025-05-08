data Tree = Empty | Node Int Tree Tree
    deriving (Show)

findTreeDepth :: Tree -> Int
findTreeDepth Empty = 0
findTreeDepth (Node _ left right) = 1 + max (findTreeDepth left) (findTreeDepth right)

countTreeElements :: Tree -> Int
countTreeElements Empty = 0
countTreeElements (Node _ left right) = 1 + countTreeElements left + countTreeElements right

countTreeLeaves :: Tree -> Int
countTreeLeaves Empty = 0
countTreeLeaves (Node _ Empty Empty) = 1
countTreeLeaves (Node _ left right) = countTreeLeaves left + countTreeLeaves right

--3 - sets the values of the tree numbers with new ints
mapTree :: (Int -> Int) -> Tree -> Tree
mapTree _ Empty = Empty
mapTree f (Node val left right) =
    Node (f val) (mapTree f left) (mapTree f right)

multiplyTree :: Int -> Tree -> Tree
multiplyTree n = mapTree (*n)

--4
foldTree :: (Int -> a -> a -> a) -> a -> Tree -> a
foldTree _ base Empty = base
foldTree f base (Node val left right) =
    f val (foldTree f base left) (foldTree f base right)


exampleTree :: Tree
exampleTree =
    Node 5
        (Node 3 Empty Empty)
        (Node 8 Empty Empty)


main :: IO ()
main = do

    let tree = Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty)
    print (inorderList tree)  -- Output should be: [3, 5, 8]
