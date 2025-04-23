data WeekDays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

isWorkingDay :: WeekDays -> Bool
isWorkingDay Saturday = False
isWorkingDay Sunday = False
isWorkingDay _ = True

main :: IO ()
main = do
    print(isWorkingDay Sunday)
