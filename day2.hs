dec :: Char -> Int
dec c = case c of
    'A' -> 1
    'B' -> 2
    'C' -> 3
    'X' -> 1
    'Y' -> 2
    'Z' -> 3
    otherwise -> 0


process :: [String] -> Int
process [] = 0
process (l:ls)
    | (dec op == dec pl) = (3 + dec pl) + process ls
    | (op == 'A' && pl == 'Y') = (dec pl + 6) + process ls
    | (op == 'A' && pl == 'Z') = (dec pl) + process ls
    | (op == 'B' && pl == 'X') = (dec pl) + process ls
    | (op == 'B' && pl == 'Z') = (dec pl + 6) + process ls
    | (op == 'C' && pl == 'X') = (dec pl + 6) + process ls
    | (op == 'C' && pl == 'Y') = (dec pl) + process ls
    | otherwise = process ls
    where
        op = l !! 0
        pl = l !! 2

part2 :: [String] -> Int
part2 [] = 0
part2 (l:ls)
    | (pl == 'X' && op == 'A') = (dec 'C') + part2 ls
    | (pl == 'Y' && op == 'A') = (dec 'A' + 3) + part2 ls
    | (pl == 'Z' && op == 'A') = (dec 'B' + 6) + part2 ls
    | (pl == 'X' && op == 'B') = (dec 'A') + part2 ls
    | (pl == 'Y' && op == 'B') = (dec 'B' + 3) + part2 ls
    | (pl == 'Z' && op == 'B') = (dec 'C' + 6) + part2 ls
    | (pl == 'X' && op == 'C') = (dec 'B') + part2 ls
    | (pl == 'Y' && op == 'C') = (dec 'C' + 3) + part2 ls
    | (pl == 'Z' && op == 'C') = (dec 'A' + 6) + part2 ls
    | otherwise = part2 ls
    where
        op = l !! 0
        pl = l !! 2



main = do
    contents <- readFile "rps.txt"
    let result = part2 (lines contents) 
    print result