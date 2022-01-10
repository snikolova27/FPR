import Data.Char
main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD"


areDuplicate :: Char -> Char -> Bool
areDuplicate x y = (x == toLower y || x == toUpper y) && x /= y 


removeAt :: String -> Int -> String
removeAt xs idx = take idx xs ++ drop (idx+2) xs

findFirstDuplicate :: String -> Int 
findFirstDuplicate xs = helper 0 xs
    where
        helper :: Int -> String -> Int
        helper _ [] = -1
        helper _ [x] = -1
        helper currentIndex (x:y:xs)
         | areDuplicate x y = currentIndex
         | otherwise = helper (currentIndex + 1) (y:xs)

reduceStr :: String -> String 
reduceStr xs
 | findFirstDuplicate xs == -1 = xs
 | otherwise = reduceStr (removeAt xs (findFirstDuplicate xs))

