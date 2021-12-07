import Data.Char
import Graphics.Rendering.OpenGL (VariableType(Bool))
main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
                                                          --    ^^                 ^^                   ^^
    --print $ removeDuplicates "dabAcCaCBAcCcaDD"
   -- print $ noDuplicates "dabCBAcaDD"
    

noDuplicates :: [Char] -> Bool 
noDuplicates = helper
 where
     helper:: [Char] -> Bool
     helper current
      | current == [] = True 
      | head current == (head $ tail current) = helper(tail $ tail current)
      | head current == toLower(head $ tail current) || head current == toUpper (head $ tail current) = False 
      | otherwise = helper (tail current)

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
reduceStr = helper
 where
     helper :: [Char] -> [Char]
     helper current 
      | noDuplicates current = current
      | otherwise = helper (removeDuplicates current)