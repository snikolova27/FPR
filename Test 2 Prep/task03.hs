main :: IO()
main = do
   print $ closestToAverage store1 == "cheese"
   print $ cheaperAlternative store2 == 1


type Product = (String,Double)
type StoreAvailability = [Product]

store1=[("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)]
store2=[("bread",1),("cheese",2.5),("bread",1),("cheese",5),("butter",2.3)]


sumPrice :: StoreAvailability -> Double 
sumPrice [] = 0
sumPrice ((_, price) : ps) = price + sumPrice ps

avg :: StoreAvailability -> Double 
avg ps = sumPrice ps / fromIntegral (length  ps)

closestToAverage :: StoreAvailability -> String
closestToAverage [] = []
closestToAverage ps = fst $ head $ filter (\ (_, price) -> abs (price - average) == minDifference) ps
    where
        average = avg ps
        minDifference = minimum (map (\ (_, price) -> abs(price - average)) ps)

cheaperThan :: Product -> StoreAvailability -> Int
cheaperThan _ [] = 0
cheaperThan p@(pName, pPrice)  ((name, price) : ps) = if pPrice < price && pName == name then 1 + cheaperThan p ps else cheaperThan p ps

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = 0
cheaperAlternative (p:ps) = cheaperThan p ps + cheaperAlternative ps
