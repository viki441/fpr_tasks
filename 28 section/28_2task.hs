--task A
sumElements :: [(Int, Int, Int)] -> [Int]
sumElements = map (\(x,y,z) -> x + y + z) 

--task B
sumEachComponent :: [(Int, Int, Int)] -> (Int, Int, Int)
sumEachComponent = foldr(\(x,y,z) (res_x, res_y, res_z) -> (res_x + x, res_y + y, res_z + z)) (0,0,0)

--task C
certainComponents :: [(Int, Int, Int)] -> Int
certainComponents = length . filter (\(x,y,z) -> x + y > z)

--task D
equalComponents :: [(Int, Int, Int)] -> Bool
equalComponents = any (\(x,y,z) -> x == y && x == z)


main :: IO ()
main = do
    print(sumElements [(1,2,2),(1,2,3)])
    print(sumEachComponent [(1,2,3), (4,5,6)])
    print(certainComponents [(1,2,3), (4,5,2)])
    print(equalComponents [(1,2,3), (4,5,2)])
