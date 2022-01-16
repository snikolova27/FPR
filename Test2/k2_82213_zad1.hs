import Data.List
import Data.Char
main :: IO()
main = do
   print $ squareDigits 9119  == 811181
   print $ squareDigits (-9119) == (-811181)
-- --    print $ numToXs 9119
-- --    print $ xsToNum [9,1,1,9]
-- --    print $ doubleXs [9,1,1,9]
-- --    print $ splitDigits [81,1,1,81]
-- --    print $ numToXs 81

numToXs :: Int -> [Int]
numToXs num =  map digitToInt (show $ abs num) 

xsToNum :: [Int] -> Int
xsToNum xs = read (map intToDigit xs)

doubleXs :: [Int] -> [Int]
doubleXs = map (^ 2)

splitDigits :: [Int] -> [Int]
splitDigits [] = []
splitDigits (x:xs) = if x < 10 then x:splitDigits xs else numToXs x  ++ splitDigits xs

squareDigits :: Int -> Int
squareDigits x 
 | x < 0 = - number
 | otherwise = number
 where
     doubled = splitDigits $ doubleXs (numToXs x)
     number = xsToNum doubled