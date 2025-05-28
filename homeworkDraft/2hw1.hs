data Laptop = Laptop {brand :: String, year :: Int, ramGB :: Int, price :: Double}
    deriving (Show, Eq, Ord)
 
laptops :: [Laptop]
laptops = [Laptop { brand = "Dell", year = 2020, ramGB = 16, price = 1500.0 },
           Laptop { brand = "Apple", year = 2021, ramGB = 8, price = 2000.0 },
           Laptop { brand = "Lenovo", year = 2020, ramGB = 32, price = 1800.0 },
           Laptop { brand = "Asus", year = 2019, ramGB = 16, price = 1200.0 },
           Laptop { brand = "Dell", year = 2021, ramGB = 8, price = 1000.0 }]

calcuteAvgPrice :: [Laptop] -> (String, Int) -> Double -> Int -> (Double, Int) 
calcuteAvgPrice [] _ sum counter = (sum, counter)
calcuteAvgPrice (l:ls) (thisBrand, thisYear) sum counter
    --произведени от тази марка след посочената година - so i am not including the year?
    | brand l == thisBrand && year l > thisYear = calcuteAvgPrice ls (thisBrand, thisYear) (sum + price l) (counter + 1)
    | otherwise = calcuteAvgPrice ls (thisBrand, thisYear) sum counter

createAvgPriceFunc :: [Laptop] -> (String, Int) -> Double
createAvgPriceFunc laptops (thisBrand, thisYear) =
    let (total, counter) = calcuteAvgPrice laptops (thisBrand, thisYear) 0.0 0
    in if counter == 0
        then 0 else total /fromIntegral counter


--task b
calcBrandPrice :: [Laptop] -> (String, Double) -> Int -> Double
calcBrandPrice laptops (thisBrand, thisPercent) amount = 
    let brandLaptops = filter (\a -> brand a == thisBrand) laptops
        selectedLaptops = take amount brandLaptops 
        prices = map (\a -> price a + thisPercent * price a) selectedLaptops
    in sum prices

getTotalPrice :: [Laptop] -> [Int] -> [(String, Double)] -> Double
getTotalPrice _ [] [] = 0
getTotalPrice laptops (x:xs) (y:ys) = calcBrandPrice laptops y x + getTotalPrice laptops xs ys
getTotalPrice _ _ _ = 0

--task c
gatherYears :: [Laptop] -> [Int] -> [Int]
gatherYears [] list = list
gatherYears (l:ls) list
    | year l `elem` list = gatherYears ls list
    | otherwise = gatherYears ls (year l : list)

calcRamForOneYear :: [Laptop] -> Int -> Int
calcRamForOneYear [] _ = 0
calcRamForOneYear (l:ls) y
    | year l == y = ramGB l + calcRamForOneYear ls y
    | otherwise   = calcRamForOneYear ls y

calcRamForEachYear :: [Laptop] -> [Int] -> [(Int, Int)]
calcRamForEachYear _ [] = []
calcRamForEachYear laptops (y:ys) =
    let total = calcRamForOneYear laptops y
    in (y, total) : calcRamForEachYear laptops ys

findMaxYear :: [(Int, Int)] -> (Int, Int)
findMaxYear [x] = x
findMaxYear (x:y:xs)
    | snd x >= snd y = findMaxYear (x:xs)
    | otherwise      = findMaxYear (y:xs)

findYear :: [Laptop] -> Int
findYear [] = -1
findYear laptops =
    let years = gatherYears laptops []
        yearRams = calcRamForEachYear laptops years
        (mostForYear, _) = findMaxYear yearRams
    in mostForYear

main :: IO ()
main = do
    let avgFunc = createAvgPriceFunc laptops
    putStrLn ("Average price: " ++ show (avgFunc ("Dell", 2019)))

    let quantities = [1,2,3,4,5]
    let markups = [("Apple", 0.5), ("Asus", 0.1), ("Dell", 0.2), ("Lenovo", 0), ("Unknown", 0)]
    putStrLn ("Total price: " ++ show (getTotalPrice laptops quantities markups))

    putStrLn ("Year with most RAM: " ++ show (findYear laptops))
