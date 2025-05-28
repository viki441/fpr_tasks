type Env = [(Char, Bool)]

evalToken :: String -> Env -> Bool
evalToken "T" _ = True
evalToken "F" _ = False
evalToken s env
    | myAll isDigit s = s /= "0"
    | length s == 1 = lookupVar (head s) env -- head s zaradi char dolu
    | otherwise = error ("Invalid token: " ++ s) -- yy 


isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs) = f x && myAll f xs


lookupVar :: Char -> Env -> Bool
lookupVar _ [] = error "Variable not found"
lookupVar c ((k,v):rest)
    | c == k = v
    | otherwise = lookupVar c rest

splitOn :: String -> [String] -> [[String]]
splitOn _ [] = []
splitOn sep xs =
    let (before, rest) = breakOn sep xs
    in before : case rest of
        [] -> []
        (_:after) -> splitOn sep after -- until it's empty

breakOn :: String -> [String] -> ([String], [String])
breakOn _ [] = ([], [])
breakOn sep (x:xs)
    | x == sep = ([], x:xs)
    | otherwise =
        let (first, second) = breakOn sep xs
        in (x:first, second)

applyNot :: [String] -> Env -> [Bool]
applyNot [] _ = []
applyNot ("!":x:rest) env = not (evalToken x env) : applyNot rest env
applyNot (x:rest) env = evalToken x env : applyNot rest env

--razdelq za & na chasti i vr bool list
evalAndPart :: Env -> [String] -> Bool
evalAndPart env tokens =
    let andGroups = splitOn "&" tokens
        evalEach = [head (applyNot t env) | t <- andGroups] 
    in foldl (&&) True evalEach

calcBoolExpr :: String -> Env -> Bool
calcBoolExpr input env =
    let tokens = words input -- words raboti kato splitva vs intervali
        orGroups = splitOn "|" tokens -- razdelq based on |
        evalEach = [ evalAndPart env group | group <- orGroups ] -- vzima razdelenite na | i gi podrazdelq na &
    in foldl (||) False evalEach

main :: IO ()
main = do
    print(calcBoolExpr "! x & ! 1 | z & y | ! 0" [('x', False), ('y', True), ('z', False)])
