simpleDecode :: [(Char, Int)] -> String
simpleDecode xs = [ ch | (ch, n) <- xs, _ <- [1..n] ]

arProgession :: Int -> Int -> Int -> [Int]
arProgession n a d = [a + (i * d) | i <- [0..(n-1)]]


main :: IO ()
main = do
    print(simpleDecode [('a',2),('b',4)])
    print (arProgession 5 1 2)

