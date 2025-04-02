import Data.Char (toLower, toUpper)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Enum, Show, Read)


--input/ output
isDayValid :: String -> Bool
isDayValid day = map toLower day `elem` validDays
  where validDays = words "monday tuesday wednesday thursday friday saturday sunday"

printDay :: String -> Maybe DayOfWeek
printDay input =
    if isDayValid input
        then Just (read (capitalize input) :: DayOfWeek)
        else Nothing
  where
    capitalize (x:xs) = toUpper x : map toLower xs
    capitalize [] = []

-- ==, <, >
compareDays :: String -> String -> String
compareDays day1 day2
    | wDay1 == wDay2 = " is equal to " 
    | wDay1 > wDay2 = " is greater than "
    | otherwise = " is smaller than " 
        where
            wDay1 = (read(day1) :: DayOfWeek)
            wDay2 = (read(day2) :: DayOfWeek)

main :: IO ()
main = do
    putStrLn "Enter today (Monday, Tuesday, etc.):"
    input <- getLine
    case printDay input of
        Just day -> putStrLn ("Today is " ++ show day)
        Nothing  -> putStrLn "Invalid input! Please enter a correct day of the week."

    putStrLn "Enter two days to compare: "
    day1 <- getLine
    day2 <- getLine
    putStrLn (day1 ++ compareDays day1 day2 ++ day2)

