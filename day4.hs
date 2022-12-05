import qualified Data.List.Split as Split

part1 :: [[String]] -> Int
part1 [] = 0
part1 (l:ls)
    | (part1Helper l) = 1 + part1 ls
    | otherwise = part1 ls


part1Helper :: [String] -> Bool
part1Helper ls = 
    -- change to foldl (||) False & rangeChk for part 2
    foldl (&&) True $ rangeChk 

    where

        rangeChk = map (\x -> x `elem` (fst chkLs)) (snd chkLs)
        chkLs = if (length ls1 > length ls2) then (ls1, ls2) else (ls2, ls1)
        ls1 = [p11 .. p12]
        ls2 = [p21 .. p22]
        p11 = read (Split.splitOn "-" (ls !! 0) !! 0) :: Int
        p12 = read (Split.splitOn "-" (ls !! 0) !! 1) :: Int
        p21 = read (Split.splitOn "-" (ls !! 1) !! 0) :: Int
        p22 = read (Split.splitOn "-" (ls !! 1) !! 1) :: Int

main = do
    contents <- readFile "pairs.txt"
    let result = part1 $ map (Split.splitOn ",") (lines contents) 
    print result