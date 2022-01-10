main :: IO ()
main = do
  print $ maximize [(\x -> x * x * x), (\x -> x + 1)] 0.5  == 1.5
  print $ maximize [(\x -> x * x * x), (\x -> x + 1)] (-2) == -8

maximize :: (Ord a, Fractional a) => [(a -> a)] -> a -> a
maximize fn x =  snd $ head $ filter ( \ (f, value) ->  abs value == maxValue) funcWithValue
 where
     funcWithValue = map (\f -> (f, (f x))) fn
     maxValue = maximum $ map (\ (_,value) -> abs value) funcWithValue