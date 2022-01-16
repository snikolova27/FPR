main :: IO()
main = do
    print $ isPerfectlyBalanced t1 == True
   -- print $ height t1
  -- print $ size t1

data BTree a = Nil | Node a (BTree a) (BTree a)
t1 = Node 'H' (Node 'a' (Node 'k' Nil Nil) (Node 'e' Nil Nil)) (Node 's' (Node 'l' Nil Nil) (Node 'l' Nil Nil))

height :: BTree a-> Int 
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

size :: BTree a-> Int 
size Nil = 0
size (Node _ left right) = 1 + size left + size right

isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced tree = res
    where
        n = height tree
        sT = size tree
        res = (2^n -1) == sT