main :: IO()
main = do
    print $ convert tree  == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))
  --  print $ sum $ getGreaterThan (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) 1
data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

convert :: BTree -> BTree
convert Nil = Nil
convert tree@(Node value left right) = helper tree tree 
 where 
     helper :: BTree -> BTree -> BTree
     helper Nil _ = Nil
     helper n@(Node value Nil Nil) originalTree = Node (sum $ getGreaterThan originalTree value) Nil Nil
     helper t@(Node value left right) originalTree =  Node (sum $ getGreaterThan originalTree value) (helper left originalTree) (helper right originalTree)


getGreaterThan :: BTree -> Int -> [Int]
getGreaterThan Nil _ = []
getGreaterThan (Node value Nil Nil) el = [value | value >= el]
getGreaterThan (Node value left right) el
 | value >= el = [value] ++ getGreaterThan left el ++ getGreaterThan right el
 | otherwise = getGreaterThan left el ++ getGreaterThan right el