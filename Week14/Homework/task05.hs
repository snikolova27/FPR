import Data.List
main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True
    print $ getListOfLevels numberBTree 0 (height numberBTree) == [[10],[5,6],[1,9,8,7],[]]
    print $ getLevelSums numberBTree == [10,11,25]
 
data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

levelSum :: BTree -> Int -> Int
levelSum tree level = sum $ getLevel tree level 

cone :: BTree -> Bool 
cone Nil = False 
cone (Node _ Nil Nil) = False
cone tree = getLevelSums tree == sort (getLevelSums tree)
 

getListOfLevels :: BTree -> Int -> Int ->  [[Int]]
getListOfLevels tree level max = if level <= max then [getLevel tree level] ++ getListOfLevels tree (level + 1) max else []
    
getLevelSums :: BTree -> [Int]
getLevelSums Nil = []
getLevelSums tree = map sum (take (len - 1) listLevels)  --we don't take the last element because it will always be the empty list
 where
     listLevels = getListOfLevels tree 0  (height tree)
     len = length listLevels


-- from previous tasks 
height :: BTree -> Int 
height Nil = 0
height (Node _ left right) = max ( 1 + height left) (1 + height right)

getLevel :: BTree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value _ _ ) 0 = [value]
getLevel (Node value left right) level =  getLevel left (level-1) ++ getLevel right (level - 1) 
