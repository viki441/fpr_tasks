simpleDecode :: [(Char, Int)] -> String
simpleDecode xs = [ ch | (ch, n) <- xs, _ <- [1..n] ]

main :: IO ()
main = do
    print(simpleDecode [('a',2),('b',4)])
