import qualified Data.Set as Set

priority :: Char -> Int
priority c = snd (filter ((== c) . fst) all !! 0)
    where
        all = (zip ['a'..'z'] [1..26]) ++ (zip ['A'..'Z'] [27..52])

part1 :: [String] -> Int
part1 [] = 0
part1 (ls:lss) = part1Help ls + part1 lss

part1Help :: String -> Int
part1Help ls = let 
    fh = Set.fromList(take (length ls `div` 2) ls)
    sh = Set.fromList(drop (length ls `div` 2) ls)
    in priority $ (Set.toList $ Set.intersection fh sh) !! 0

part2 :: [String] -> Int
part2 (a:b:c:[]) = part2Help a b c
part2 (a:b:c:ls) = (part2Help a b c) + part2 ls

part2Help :: String -> String -> String -> Int
part2Help a b c = let
    inter1 = Set.intersection (Set.fromList a) (Set.fromList b)
    inter2 = Set.toList $ Set.intersection (inter1) (Set.fromList c)
    in priority $ inter2 !! 0

main = do
    contents <- readFile "types.txt"
    let result = part2 (lines contents) 
    print result