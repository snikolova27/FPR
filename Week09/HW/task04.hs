main :: IO()
main = do
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97] 


isPrime:: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = not ((length [x | x <- [2 .. n-1], mod n x == 0]) > 0)

primesInRange:: Int-> Int -> [Int]
primesInRange a b = [x| x<- [min a b .. max a b], isPrime x]