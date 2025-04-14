sortGrades :: [(String, Double)] -> [(String, Double)]
sortGrades = foldr insertSorted []
    where
        insertSorted x@(name, grade) res =
            case res of
                []     -> [x]
                (r@(n, gr):rs) ->
                    if gr > grade || (gr == grade && n < name)
                        then r:insertSorted x rs
                        else x:res 
main :: IO ()
main = do
  print  (sortGrades  [("ana", 6), ("ivan", 6), ("gosho", 4)])
