import Data.List
main :: IO()
main = do
    print $ isBST t == True

data BTree = Nil | Node Int BTree BTree
 deriving (Show)

t = Node 7 (Node 3 (Node 1 Nil Nil) (Node 5 Nil Nil)) (Node 10 Nil (Node 14 Nil Nil))

isBST :: BTree -> Bool
isBST t = let nodes = traverseDFS t in nodes == sort nodes

traverseDFS :: BTree -> [Int]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right
