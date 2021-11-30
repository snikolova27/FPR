main::IO()
main = do
    print $ fibRec 11 == 89
    print $ fibIter 11 == 89
    print $ fibIter 110 == 43566776258854844738105

fibRec :: Int->Int
fibRec 0 = 0
fibRec 1 = 1
fibRec x = fibRec (x - 1) + fibRec(x - 2)

fibIter::Integer->Integer 
fibIter x = helper 0 1 x
 where
     helper::Integer->Integer->Integer->Integer
     helper twoAgo _ 0 = twoAgo
     helper _ oneAgo 1 = oneAgo
     helper twoAgo oneAgo leftOver = helper oneAgo (twoAgo + oneAgo) (leftOver - 1)



