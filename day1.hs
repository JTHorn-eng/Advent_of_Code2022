import System.IO 
import Control.Monad 
import qualified Data.Char as Char


process :: [String] -> Int -> [Int] -> [Int]
process [] _ res = res
process (l:ls) acc res
    | (l /= "") = process ls (acc + (f l)) res
    | otherwise = process ls 0 (res ++ [acc])
    where
        f r = read r :: Int

removeFromList :: (Eq a) => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList elem (l:ls)
    | (l == elem) = removeFromList elem ls
    | otherwise = [l] ++ (removeFromList elem ls)

topthree :: [Int] -> [Int]
topthree ls = take 3 $ [m] ++ (topthree $ removeFromList m ls)
    where
        m = maximum ls

main = do
    contents <- readFile "calories.txt"
    let result = process (lines contents) 0 []
        output = foldl (+) 0 (topthree result)
    print output

