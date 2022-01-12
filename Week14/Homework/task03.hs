main :: IO()
main = do

    print $ toBinaryIndexed tree   == Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))
   -- print $ findPosInDFS (traverseDFS tree) 7 0
   
data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value Nil Nil) = [value]
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right 

findPosInDFS  :: (Eq a) => [a] -> a -> Int -> Int
findPosInDFS [] _ _ = error "element not in list"
findPosInDFS (x:xs) element current
 | element == x = current
 | otherwise = findPosInDFS xs element (current  + 1)

toBinaryIndexed ::  (Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil
toBinaryIndexed tree@(Node value left right) = helper tree tree 
 where 
     helper :: (Eq a) => BTree a-> BTree a -> BTree (a, Int)
     helper Nil _ = Nil
     helper (Node value left right) ogTree =  Node (value, findPosInDFS (traverseDFS ogTree) value 0) (helper left ogTree) (helper right ogTree)
  
  
