import Data.Char (toLower, toUpper)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Enum, Show, Read, Bounded)

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
compareDays day1 day2 = case (printDay day1, printDay day2) of
    (Just d1, Just d2) -> compareDayValues d1 d2 
    _ -> " is not related to "
  where
        compareDayValues d1 d2
            | d1 == d2 = " is equal to " 
            | d1 > d2 = " is greater than "
            | otherwise = " is smaller than "

--find previous and next day, by given day
previousDay :: String -> String
previousDay day = case (printDay day) of
    Just d -> show $ if d == minBound then maxBound else pred d
    Nothing -> ""

nextDay :: String -> String
nextDay day = case printDay day of
    Just d -> show $ if d == maxBound then minBound else succ d
    Nothing -> ""

findDays :: String -> (String , String)
findDays day
    | isDayValid day == False = ("", "")
    | otherwise = (previousDay day, nextDay day)
     
--turn in number
turnInNumber :: String -> Int
turnInNumber day = case printDay day of
    Just d -> fromEnum d + 1
    Nothing -> 0

--turn from number
turnFromNumber :: Int -> Maybe DayOfWeek
turnFromNumber num
    | num >= 1 && num <= 7 = Just (toEnum (num - 1) :: DayOfWeek)
    | otherwise = Nothing


--interval of days
intervalOfDays :: String -> String -> [String]
intervalOfDays start end = case (printDay start, printDay end) of
    (Just s, Just e) ->
        let sEl = fromEnum s
            eEl = fromEnum e
            indices = if sEl <= eEl then [sEl .. eEl] else [sEl .. 6] ++ [0 .. eEl]
        in map (show . (toEnum :: Int -> DayOfWeek)) indices
    _ -> []


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

    putStrLn "Enter a day you want to find pred and succ: "
    day3 <- getLine
    putStrLn ("previous and next: "  ++ show(findDays day3))

    putStrLn "Turn day into int: "
    day4 <- getLine
    putStrLn (show(turnInNumber day4))

    putStrLn "Enter a number between 1 and 7"
    num1 <- getLine
    let number = read num1 :: Int
    putStrLn (show(turnFromNumber number))

    putStrLn "Enter two days to make interval: "
    day5 <- getLine
    day6 <- getLine
    putStrLn (show (intervalOfDays day5 day6))
