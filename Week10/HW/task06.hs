main :: IO()
main = do
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
    print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 6] == (False, 0)
    print $ isImage [1, 2] [-1, -2] == (False, 0)
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 4] == (False, 0)

findN :: (Num a) => [a] -> [a] -> a 
findN [] _ = error "First list was empty"
findN _ [] = error "Second list was empty"
findN (x:xs) (y:ys) = y - x 

isImage ::(Num a, Eq a) => [a] -> [a]-> (Bool, a)
isImage [] [] = (True, 0)
isImage [] _ = error "Second list was empty"
isImage _ [] = error "First list was empty"
isImage xs ys = if map (+ n) xs == ys then (True, n) else (False,0)
 where
     n = findN xs ys 
