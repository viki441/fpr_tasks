pivotl :: [Int] -> Int -> ([Int], [Int])
pivotl [] _ = ([], [])
pivotl list y = (filter (< y) list, filter (>= y) list)

main :: IO ()
main = do
    print (pivotl [2,3,4,5,6,2,4,2,4,1,3,4] 3)
