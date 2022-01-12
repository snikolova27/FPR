import Data.List
main :: IO()
main = do

    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False
    --print $ getLeaves t2

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))
t3 = t1
t4 =  Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

getLeaves :: BTree -> [Int]
getLeaves Nil = []
getLeaves (Node value Nil Nil) = [value]
getLeaves (Node value left right) = getLeaves left ++ getLeaves right

leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual _ Nil = False 
leavesAreEqual Nil _ = False 
leavesAreEqual Nil Nil = True
leavesAreEqual (Node v1 Nil Nil) (Node v2 Nil Nil) = v1 == v2
leavesAreEqual t1 t2 =  (sort $ getLeaves t1) ==  (sort$ getLeaves t2)

