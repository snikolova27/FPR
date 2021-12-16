main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)
    -- print $ getX [(1,2) , (1,2)] 0
    -- print $ getX [(4,2) , (1,2)] 0
    -- print $ getX [(3, 9), (8, 7), (7, 9)] 0
    -- print $ getX [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] 0
    -- print $ getY [(1,2) , (3,2)] 0
    -- print $ getY [(4,2) , (1,2)] 0
    -- print $ getY [(3, 9), (8, 7), (7, 9)] 0
    -- print $ getY [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] 0

type Vector = (Int, Int)

getX :: [Vector] -> Int -> Int
getX [] current = current
getX (x:xs) current = if fst x <= snd x then getX xs (10* current + fst x) else getX xs (10* current + snd x)

getY :: [Vector] -> Int -> Int
getY [] current = current
getY (x:xs) current = if fst x >= snd x then getY xs (10* current + fst x) else getY xs (10* current + snd x)

    

combine :: [Vector] -> Vector
combine [] = error "List was empty"
combine [x] = if fst x < snd x then (snd x, fst x) else x
combine xs = (getX xs 0, getY xs 0)

