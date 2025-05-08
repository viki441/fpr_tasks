separate :: (a -> Bool) -> [a] -> ([a], [a])
separate p xs = (pref, suf)
    where
        (pref, suf) = break (not . p) xs

main :: IO ()
main = do
    print(separate even [2,4,6,8,10,2,3,2,6])
