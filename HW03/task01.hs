import Data.List
main :: IO ()
main = do
    print $ willItFly [1, 4, 2, 3] == True -- |1-4|=3,|4-2|=2,|2-3|=1
    print $ willItFly [1, 4, 2, -1, 6] == False
   -- print $ getListOfAbs [1,4,2,3]

getListOfAbs :: [Int] -> [Int]
getListOfAbs [] = []
getListOfAbs [x,y] = [abs (x-y)]
getListOfAbs (x:y:xs) = abs (x-y) : getListOfAbs (y :xs)

willItFly :: [Int] -> Bool
willItFly [] = True 
willItFly xs = sort (getListOfAbs xs) == [1.. (length xs - 1)]

