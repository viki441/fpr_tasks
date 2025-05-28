data BinTree = Nil | Node Int BinTree BinTree
    deriving (Show, Eq)

data Tree = Nil1 | Node1 Int [Tree]
    deriving (Show, Eq)

tree :: BinTree
tree = Node 5 (Node 4 Nil Nil)
              (Node 3 (Node 1 Nil Nil)
                      (Node 2 Nil Nil))

tree2 :: BinTree
tree2 = Node 5 (Node 4 (Node 8 Nil Nil) Nil)
               (Node 3 (Node 1 Nil Nil)
                       (Node 2 Nil Nil))

--task A
areEqual :: BinTree -> BinTree -> Bool
areEqual Nil Nil = True
areEqual _ Nil = False
areEqual Nil _ = False
areEqual treeOne treeTwo = if treeOne == treeTwo then True else False


--task B
mirrorTree :: BinTree -> BinTree
mirrorTree Nil = Nil -- diff nil
mirrorTree (Node val left right) = Node val (mirrorTree right) (mirrorTree left)

--task C
newTree :: Tree
newTree = Node1 5 [Node1 6 [],
               Node1 7 [Node1 10 [], Node1 11 []],
               Node1 8 [Node1 9 []]]

alignVal :: String -> [String] -> [String]
alignVal _ [] = []
alignVal _ [x] = [x]
alignVal sep (x:xs) = x : sep : alignVal sep xs

stringifyTree :: Tree -> String
stringifyTree Nil1 = "[]"
stringifyTree (Node1 val []) = "[" ++ show val ++ "]"
stringifyTree (Node1 val children) 
        = "[" ++ show val ++ ",[" ++ childrenStr ++"]]"
    where
        childrenStr = concat (alignVal "," (map stringifyTree children))

--task D -- use previous Tree
--group children under each parent
groupChildren :: [(Int, Int)] -> [(Int, [Int])]
groupChildren [] = []
groupChildren ((p,c):xs) = insertChild p c (groupChildren xs)

insertChild :: Int -> Int -> [(Int, [Int])] -> [(Int, [Int])]
insertChild p c [] = [(p, [c])]
insertChild p c ((x,cs):rest)
    | x == p = (x, c:cs) : rest
    | otherwise = (x,cs) : insertChild p c rest

--find the root
findRoot :: [(Int, Int)] -> Int
findRoot pairs =
    let parents = map fst pairs
        children = map snd pairs
    in head [p | p <- parents, not (p `elem` children)]

--build the tree
buildSubtree :: Int -> [(Int, [Int])] -> Tree
buildSubtree val table =
    case lookup val table of
        Nothing -> Node1 val []
        Just children -> Node1 val [buildSubtree c table | c <- children]

buildTree :: [(Int, Int)] -> Tree
buildTree pairs =
    let grouped = groupChildren pairs
        root = findRoot pairs
    in buildSubtree root grouped


main :: IO ()
main = do
    -- some trees for debugging
    let tree3 = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)
    let tree4 = Node 1 (Node 2 Nil Nil) (Node 4 Nil Nil)
    let tree5 = Node 1 (Node 3 Nil (Node 8 Nil Nil)) (Node 2 Nil Nil)

    print(stringifyTree (buildTree [(8,9),(5,3),(5,4),(1,5),(8,2),(1,8),(8,6)]))

