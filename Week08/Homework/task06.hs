main::IO()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

cntDivisors::Int->Int
cntDivisors 1 = 1
cntDivisors 2 = 2
cntDivisors x = helper x 0
 where
     helper::Int->Int->Int
     helper currentX cntDivs
      |currentX == 0 = cntDivs
      |mod x currentX == 0 = helper (currentX - 1) (cntDivs+ 1)
      | otherwise = helper (currentX-1) cntDivs


isPrimeG::Int->Bool
isPrimeG 1 = False
isPrimeG 2 = True
isPrimeG x = cntDivisors x <= 2 


isPrimeLC::Int->Bool
isPrimeLC x = length([n| n <- [1 .. x], mod x n == 0]) == 2

