import Data.Char
main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    print $ containsD 123 3

isPrime:: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = not ((length [x | x <- [2 .. n-1], mod n x == 0]) > 0)

containsD :: Int-> Int -> Bool
containsD n d = elem (intToDigit d) (show n)

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [x | x<- [2..], containsD x d && isPrime x]