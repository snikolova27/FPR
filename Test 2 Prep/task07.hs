main :: IO()
main = do
   print $ inverseFun (\x -> x+1) (\x -> x-1) 5 10  ==True
   print $ inverseFun (\x -> x*x) (\x -> x^3) 0 1  == True
   print $ inverseFun (\x -> x+1) (\x -> x+2) 0 1  ==  False

inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b = map (f.g) [a..b] == [a..b] && map (g.f) [a..b] == [a..b]

-- [5,6,7,8,9,10]
-- [0,1]
-- [3,4]