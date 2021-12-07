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
getListOfDuplicates xs = helper [] xs
 where
     helper :: [Int] -> [Int] -> [Int]
     helper current [] = current
     helper current leftOver
      | cntOccurences leftOver (head leftOver) > 1 = helper (current ++ [head leftOver]) (tail leftOver)
      | otherwise = helper current (tail leftOver)




sumUnique :: [[Int]] ->Int 
sumUnique xss = helper xss 0 (head xss) (getListOfDuplicates (head xss))
 where
     helper:: [[Int]] -> Int -> [Int] -> [Int] -> Int
     helper [] sum  currentXs currentDups = sum
     helper leftOver sum currentXs currentDups
      | currentXs == [] = helper (tail leftOver) sum (head $ tail leftOver) (getListOfDuplicates (head $ tail leftOver))
      | notElem (head currentXs) currentDups = helper leftOver (sum + head currentXs) (tail currentXs) currentDups 
      | otherwise = helper leftOver sum (tail currentXs) currentDups