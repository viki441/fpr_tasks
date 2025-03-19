import Data.Char (ord, chr, isLower, isUpper)

transformString :: String -> String
transformString [] = []
transformString (x:xs)
  | isLower x = chr (ord x - 32) : transformString xs  
  | isUpper x = chr (ord x + 32) : transformString xs  
  | otherwise = transformString xs  

main :: IO ()
main = do
  print (transformString ['s', 'A', '3']) 
