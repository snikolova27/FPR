import Distribution.Verbosity (normal)
main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True

type Rat = (Int, Int)

normalizeWhere :: Rat -> Rat
normalizeWhere (x, 0) = error "can't divide by zero"
normalizeWhere (x, y) = (div x d, div y d)
 where d = gcd x y

sumRats :: Rat -> Rat -> Rat
sumRats (_,0) (_,_) = error "Zero cannot be in the denominator"
sumRats (_,_) (_,0) = error "Zero cannot be in the denominator"
sumRats (x1,y1) (x2,y2) = if y1 == y2 then  normalizeWhere  (x1+x2,y1) else normalizeWhere (x1 * y2 + x2 * y1,d)
 where d = y1 * y2

multiplyRats :: Rat -> Rat -> Rat
multiplyRats (_,0) (_,_) = error "Zero cannot be in the denominator"
multiplyRats (_,_) (_,0) = error "Zero cannot be in the denominator"
multiplyRats (x1,y1) (x2,y2) = normalizeWhere (x1*x2,y1*y2)

divideRats :: Rat -> Rat -> Rat
divideRats (_,0) (_,_) = error "Zero cannot be in the denominator"
divideRats (_,_) (_,0) = error "Zero cannot be in the denominator"
divideRats (x1,y1) (x2,y2) = normalizeWhere (x1*y2,x2*y1)

areEqual :: Rat -> Rat -> Bool 
areEqual (_,0) (_,_) = error "Zero cannot be in the denominator"
areEqual (_,_) (_,0) = error "Zero cannot be in the denominator"
areEqual (x1,y1) (x2,y2) =  normalizeWhere (x2,y2) == normalizeWhere (x1,y1)