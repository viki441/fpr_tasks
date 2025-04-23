import Data.List (sortBy)
import Data.Ord (comparing)

data Genre = Fiction | Comedy | Horror
    deriving (Show, Eq)

data Book = Book {name::String, author::String, genre::Genre, dateYear::Int}
    deriving (Show, Eq)

data LibraryTree = Empty | Node (Book, Int) LibraryTree LibraryTree
    deriving (Show)

--data
book1 = Book "Dracula" "Bram Stoker" Horror 1897
book2 = Book "The Hitchhiker's Guide to the Galaxy" "Douglas Adams" Comedy 1979
book3 = Book "The 90's?" "Yours true and only, me" Comedy 2024
book4 = Book "1984" "George Orwell" Fiction 1949

sampleTree :: LibraryTree
sampleTree = Node (book1, 4)
                (Node (book2, 7) Empty Empty) 
                (Node (book3, 0) Empty (Node (book4, 1)Empty Empty))

countBooks :: LibraryTree -> Int
countBooks (Empty) = 0
countBooks (Node (_, copies) l r ) = copies + countBooks l + countBooks r

genreBooks :: Genre -> LibraryTree -> [String]
genreBooks _ Empty = []
genreBooks g (Node (book, _)l r)
    |genre book == g = author book : rest
    |otherwise = rest
    where
        rest = genreBooks g l ++ genreBooks g r

--order the books
flatten :: LibraryTree -> [(Book, Int)]
flatten Empty = []
flatten (Node b l r) = flatten l ++ [b] ++ flatten r

sortBooks :: [(Book, Int)] -> [(Book, Int)]
sortBooks = sortBy (comparing (name . fst))

main :: IO ()
main = do
    putStrLn "Count of books: "
    print (countBooks sampleTree)
    putStrLn "Authors from a genre: "
    print (genreBooks Comedy sampleTree)
    putStrLn "Sorted tree: "
    let sorted = sortBooks (flatten sampleTree)
    mapM_ (print . fst) sorted
