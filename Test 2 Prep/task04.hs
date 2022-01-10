main :: IO()
main = do

    print $ distanceBetween (1,1,1) [(1,1,1),(2,3,3), (5,4,5)]
    print $ minDistance [(1,1,1),(2,3,3), (5,4,5)]

distanceBetween :: (Double,Double,Double) -> [(Double,Double,Double)] -> [Double]
distanceBetween _ [] = []
distanceBetween point@(x1,y1,z1) ((x2,y2,z2):ps) = if x1 /= x2 && y1 /= y2 && z1 /= z2 then [sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2))] ++ distanceBetween point ps else distanceBetween point ps 

minDistance :: [(Double,Double,Double)] -> Double
minDistance [] = 0
minDistance points@ (p:ps) = minimum $ concat $ allDist points points
 where
     allDist :: [(Double,Double,Double)] -> [(Double,Double,Double)] -> [[Double]]
     allDist [] _ = []
     allDist (p:ps) points = distanceBetween p points : allDist ps points