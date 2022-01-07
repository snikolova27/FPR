main :: IO()
main = do
    print $ getAverage db1 == 4.457142857142857

    print $ getNeeded 750 db1 == [Product "Cheese" 750 5.0,Product "Water" 500 0.5,Product "Soap" 250 4.5]

    print $ closestToAverage db1 == ["Milk","Soap"]

    print $ cheaperAlternatives "Lamb" 5.50 db2 == 1
    print $ cheaperAlternatives "Lamb" 10  db2 == 2


type Name = String
type Quantity = Int
type Price = Double
type Database = [Product]

data Product = Product Name Quantity Price 
 deriving (Show, Eq)

db1 :: Database
db1 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Butter" 1000 5.50, Product "Water" 500 0.50, Product "Soap" 250 4.50 ]

db2 :: Database
db2 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Lamb" 1000 5.50, Product "Water" 500 0.50, Product "Lamb" 250 4.50 ]

getTotal :: Database -> Price
getTotal = foldl (\ acc (Product _ _ p) -> p + acc) 0

getAverage :: Database -> Price
getAverage db  = getTotal db / fromIntegral (length db)

getNeeded :: Int -> Database -> Database
getNeeded _ [] = []
getNeeded quantity (p@(Product pName pQuantity pPrice) : ps) 
 | pQuantity <= quantity = p : getNeeded quantity ps
 | otherwise  = getNeeded quantity ps


closestToAverage ::  Database -> [Name]
closestToAverage [] = []
closestToAverage db = map (\ (Product name _ _) -> name) (filter( \ (Product _ _ pPrice) -> abs(pPrice - avg) == minDiff) db)
 where 
     avg = getAverage db
     minDiff  = minimum (map (\ (Product _ _ price) -> abs(price - avg)) db)

cheaperAlternatives :: Name -> Price -> Database -> Int 
cheaperAlternatives _ _ [] = 0
cheaperAlternatives name price (p@(Product pName _ pPrice) : ps)
 | pName == name && price > pPrice = 1 + cheaperAlternatives name price ps
 | otherwise = cheaperAlternatives name price ps