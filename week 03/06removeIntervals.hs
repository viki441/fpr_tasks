removeIntervals :: String -> String
removeIntervals s =
  let trimmed = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse $ s 
  in compressSpaces trimmed

compressSpaces :: String -> String
compressSpaces [] = []
compressSpaces (x:xs) =
  x : [ curr | (prev, curr) <- zip (x:xs) xs, not (prev == ' ' && curr == ' ') ]
