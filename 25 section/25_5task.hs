toUpperCase :: Char -> Char
toUpperCase c = if c >= 'a' && c <= 'z' 
                --Converts an enumerated type (like Char) into an Int representation
                then toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A') :: Char 
                --Char:: It tells Haskell that the result of toEnum should be interpreted as a Char.
                else c

convertString :: String -> String
convertString = map toUpperCase

--OR
import Data.Char (toUpper)

convertString :: String -> String
convertString = map toUpper


main :: IO ()
main = do
    print (convertString "Hello, World!")
