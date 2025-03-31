say :: Int -> String
say n 
    | n >= 0 && n <= 9 = words "zero one two three four five six seven eight nine" !! n
    | otherwise = "Invalid input"

main :: IO ()
main = do
    print (say 3)
