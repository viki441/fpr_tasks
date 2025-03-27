reverseDigits :: Int -> [Int]
reverseDigits n = [read [c] | c <- reverse (show n)] 

main :: IO ()
main = do
    print (reverseDigits 123)
