decodeL :: [(Int, b)] -> [b]
decodeL [] = []
decodeL list =  [ b | (a, b) <- list, _ <-[1..a]]

main :: IO ()  
main = do
    print (decodeL [(4,'h'), (2, 't'), (5, 'q')])  
