main :: IO()
main = do
    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
-- same for removeAllHOF
    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
   
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll y [x] =  [x | x /= y]
removeAll y (x:xs) = if y == x then removeAll y xs else x : removeAll y xs 

removeAllHOF :: Int -> [Int] -> [Int]
removeAllHOF _ [] = []
removeAllHOF y [x] =  [x | x /= y]
removeAllHOF y xs = filter ( /= y) xs


