main::IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False

hasIncDigits::Int->Bool
hasIncDigits x
 |x < 0 = error "x should be non-negative"
 |x < 10 = True
 | mod x 10 >= mod (div x 10) 10 = hasIncDigits(div x 10)
 |otherwise = False