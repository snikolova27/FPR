main :: IO()
main = do

    print $ calc [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998

calc :: [Double] -> (Double -> Int -> Double )
calc list = (\ x y -> foldl (\acc c -> acc * (f x c)) 1 (take y list))
 where
     f:: Double -> Double -> Double
     f x y = x - y
