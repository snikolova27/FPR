import Data.List
import GHC.Exts (currentCallStack)
main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9  
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45

   -- print $ areAllTheSame [2,2,2]
    --print $ cntOccurences [2,2,2,2,2,2,2,2,2] 2
   -- print $ getListOfDuplicates [1,2,3,2,-4,-4,5]

-- areAllTheSame :: [Int] -> Bool
-- areAllTheSame xs = elem (head xs) (nub xs)

cntOccurences :: [Int] -> Int -> Int
cntOccurences [] x = 0
cntOccurences leftOver x  = if elem x leftOver then  1 + cntOccurences(tail leftOver) x  else  cntOccurences (tail leftOver) x 

getListOfDuplicates :: [Int] -> [Int]
getListOfDuplicates [] = []
getListOfDuplicates (x:xs)
 | elem x xs = x : getListOfDuplicates xs
 | otherwise = getListOfDuplicates xs




sumUnique :: [[Int]] ->Int 
sumUnique (x:xs) = helper (x:xs) x (getListOfDuplicates x)
 where
     helper:: [[Int]] -> [Int] -> [Int] -> Int
     helper [] _ _ = 0
     helper leftOver [] currentDups = helper (tail leftOver) (head $ tail leftOver) (getListOfDuplicates (head $ tail leftOver))
     helper leftOver (x:xs) currentDups
      | notElem x currentDups =  x + helper leftOver xs currentDups 
      | otherwise = helper leftOver xs currentDups