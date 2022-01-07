import Data.List
main :: IO()
main = do
    print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == t2
    print $ getLeft [3, 2, 1, 6, 0, 5] == [3,2,1]
    print $ getRight [3, 2, 1, 6, 0, 5] == [0,5]

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

t2 = Node 6 ( Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil )



constructMaxBTree :: [Int] -> BTree Int
constructMaxBTree [] = Nil
constructMaxBTree nodes = Node max (constructMaxBTree left) (constructMaxBTree right)
 where
     max = maximum nodes
     left = getLeft nodes
     right = getRight nodes

getLeft :: [Int] -> [Int] 
getLeft [] = []
getLeft l@(x:xs)
 | x < max = x : getLeft xs
 | otherwise = []
  where 
      max = maximum l

getRight :: [Int] -> [Int]
getRight [] = []
getRight l = let max = maximum l in l \\ (getLeft l ++ [max])