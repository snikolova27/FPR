main::IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

sumDiv::Int->Int
sumDiv x = helper x 0
 where 
     helper::Int->Int->Int
     helper currentX sum
      | currentX == 0 = sum
      | mod x currentX == 0 = helper (currentX - 1) (sum + currentX)
      | otherwise = helper (currentX - 1) sum 


areAmicable::Int->Int->Bool
areAmicable x y = sumDiv x == sumDiv y
