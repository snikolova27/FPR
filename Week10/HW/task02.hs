main::IO()
main = do
    print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    --print $ removeFirst 5 [5, 1, 5, 3, 5]

removeFirst :: Int -> [Int] -> [Int]
removeFirst _ [] = []
removeFirst y (x:xs) = if y == x then xs else  x : removeFirst y xs