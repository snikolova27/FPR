import Data.Char
main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
                                                          --    ^^                 ^^                   ^^
    --print $ removeDuplicates "dabAcCaCBAcCcaDD"
   -- print $ noDuplicates "dabCBAcaDD"
    

noDuplicates :: [Char] -> Bool 
noDuplicates [] = True 
noDuplicates [x] = True 
noDuplicates (x:y:xs)
 | x == y = noDuplicates xs
 | x == toLower y || x == toUpper y = False 
 | otherwise = noDuplicates (y:xs)

removeDuplicates :: [Char]-> [Char]
removeDuplicates xs = helper xs []
 where
     helper :: [Char] -> [Char] -> [Char]
     helper [] res = res
     helper leftOver res
      | head leftOver == (head $ tail leftOver) = helper (tail $ tail leftOver) (res ++ [head leftOver]++ [head $ tail leftOver])
      | head leftOver /= (head$ tail leftOver) && (head leftOver == (toLower(head $ tail leftOver))) || (head leftOver == (toUpper (head $ tail leftOver))) = helper (tail $ tail $ leftOver) res
      | otherwise = helper (tail leftOver) (res ++ [head leftOver])

reduceStr :: [Char] -> [Char]
reduceStr current
      | noDuplicates current = current
      | otherwise = reduceStr (removeDuplicates current)