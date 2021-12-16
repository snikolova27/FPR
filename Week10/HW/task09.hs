main :: IO()
main = do
    print $ naturalProduct [-1, 0, -2, -3] 5 == 0 -- There are no natural numbers
    print $ naturalProduct [5, 10] 5 == 0 -- Sum of the divisors is 9
    print $ naturalProduct [95, 75, 15, 55, 11, 14, 18, 35, 25] 5 == 1330

getListOfDivNatural :: Int -> [Int]
geListOfDivNatural 0 = error "Not a natural number" 
getListOfDivNatural x =  if x > 0 then [k | k <- [1..x-1], mod x k == 0 ] else error "Not a natural number"

getListOfSumsOfDivs :: [Int] -> [Int]
getListOfSumsOfDivs [] = []
getListOfSumsOfDivs xs =  [ sum $ getListOfDivNatural x | x <- xs ]

areAllNatural :: [Int] -> Bool
areAllNatural [] = True 
areAllNatural [x] = x > 0
areAllNatural xs =  length [x | x <- xs, x > 0] == length xs

naturalProduct :: [Int] -> Int  -> Int
naturalProduct [] _ = 0
naturalProduct xs k = if areAllNatural xs && not (null ( [x | x <-xs, mod (current x) k == 0 ])) then  product [x | x <-xs, mod (current x) k == 0 ] else 0 
 where
     current :: Int -> Int
     current = sum.getListOfDivNatural 