import Data.List
main :: IO()
main = do
    print $ isSorted [-5, -5, -6] == True
    print $ isSorted [-5, -5, -4] == True
    print $ isSorted [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSorted [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSorted [-100, -99, -99, -99] == True
    print $ isSorted [-100, -99, -99, -99, 100] == True
    print $ isSorted [100, 101, -102] == False
    print $ isSorted [1, 2, 3, 4, 5, 6] == True
    print $ isSorted [-1, -2, -3, -4, -5, -6] == True
   
    print $ isSortedXs [-5, -5, -6] == True
    print $ isSortedXs [-5, -5, -4] == True
    print $ isSortedXs [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSortedXs [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSortedXs [-100, -99, -99, -99] == True
    print $ isSortedXs [-100, -99, -99, -99, 100] == True
    print $ isSortedXs [100, 101, -102] == False
    print $ isSortedXs [1, 2, 3, 4, 5, 6] == True
    print $ isSortedXs [-1, -2, -3, -4, -5, -6] == True

    -- same for isSortedXs
 
isSortedInAscending :: (Ord a) => [a] -> Bool 
isSortedInAscending [] = True 
isSortedInAscending [x] = True
isSortedInAscending (x:y:xs) =  (x <= y) && isSortedInAscending (y:xs)

isSortedInDescending :: (Ord a) => [a] -> Bool 
isSortedInDescending [] = True 
isSortedInDescending [x] = True
isSortedInDescending (x:y:xs) =  (x >= y) && isSortedInDescending (y:xs) 

isSorted ::(Ord a) => [a] -> Bool 
isSorted [] = True 
isSorted [x] = True 
isSorted xs = isSortedInAscending xs || isSortedInDescending xs

isSortedXs :: [Int] -> Bool
isSortedXs [] = True 
isSortedXs [x] = True
isSortedXs xs = xs == sort xs || (reverse $ sort xs) == xs 