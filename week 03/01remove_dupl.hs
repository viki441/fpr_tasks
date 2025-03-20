remove_duplicates :: [Int] -> [Int]
remove_duplicates xs = remove_helper xs []
  where
    remove_helper [] _ = []
    remove_helper (y:ys) seen =
        {-
        1vo vojdame dali y e v seen, posle ako ne e:
        trqbva da go dobavim kum seen
        y ++ remove_helper ys seen--->(proverka za notelem i vmukvane v seen)
        -}
      [ y | y `notElem` seen ] ++ remove_helper ys (if y `notElem` seen then y:seen else seen)

main :: IO ()
main = do
    print(remove_duplicates[5,1,2,3,4,4,3,3,2,1])
