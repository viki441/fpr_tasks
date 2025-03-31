toUpperCase :: Char -> Char
toUpperCase c = if c >= 'a' && c <= 'z' 
                then toEnum (fromEnum c - 32) 
                else c

convertString :: String -> String
convertString = map toUpperCase

main :: IO ()
main = do
    print (convertString "Hello, World!")
