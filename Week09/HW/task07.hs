import Data.Char
import Data.List
main :: IO()
main = do
    print $ duplicateCount "" == 0 -- no characters repeats more than once
    print $ duplicateCount "abcde" == 0
    print $ duplicateCount "aabbcde" == 2 -- 'a' and 'b'
    print $ duplicateCount "aabBcde" == 2 -- 'a' occurs twice and 'b' twice (`b` and `B`)
    print $ duplicateCount "indivisibility" == 1 -- 'i' occurs six times
    print $ duplicateCount "Indivisibility" == 1
    print $ duplicateCount "aA11" == 2 -- 'a' and '1'
    print $ duplicateCount "ABBA" == 2 -- 'A' and 'B' each occur twice
    print $ duplicateCount "Indivisibilities" == 2 -- 'i' occurs seven times and 's' occurs twice
    print $ duplicateCount ['a'..'z'] == 0
    print $ duplicateCount (['a'..'z'] ++ ['A'..'Z']) == 26

cntOccurences :: [Char] -> Char -> Int
cntOccurences leftOver x  = length $ filter (==x) leftOver 

getListOfDuplicates :: [Char] -> [Char]
getListOfDuplicates [] =[]
getListOfDuplicates (x:xs)
 | elem x xs = x: getListOfDuplicates xs
 | otherwise = getListOfDuplicates xs

duplicateCount :: [Char] -> Int 
duplicateCount xs = length $ nub $ getListOfDuplicates $ map toLower xs