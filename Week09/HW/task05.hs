import Data.Char
main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6

    -- print $ reverseN 123
    -- print $ getAscendingSubList [3,6,5,7,3]
    -- print $ getAscendingSubList [3,6,7,2,3]
    -- print $ getAscendingSubList [6,6,6,2,3]

    -- print $ xsToNum [6,6,6,6]

reverseN :: Int -> [Int] 
reverseN n =  map digitToInt $reverse $ show n

getAscendingSubList :: [Int] -> [Int]
getAscendingSubList xs = helper [head xs] xs
 where
     helper :: [Int] -> [Int] -> [Int]
     helper current [] = current
     helper current leftOver = if (head leftOver) < (head $tail leftOver) then helper (current ++ [head $ tail leftOver]) (tail leftOver) else  current

xsToNum :: [Int] -> Int
xsToNum = read .map intToDigit

reverseOrdSuff :: Int -> Int
reverseOrdSuff n =  xsToNum $ getAscendingSubList $ reverseN n