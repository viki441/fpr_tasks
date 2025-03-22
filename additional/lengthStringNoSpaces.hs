main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine
    let letterCount = length [c | c <- input, (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')]
    putStrLn ("Number of letters: " ++ show letterCount)
{-ghci 
:load file.hs
main
input string-}
