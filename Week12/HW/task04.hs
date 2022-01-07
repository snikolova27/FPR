main :: IO()
main = do
    --print t1
    --print t2
    print $ isGracefull t1 == True -- t1 = A
    print $ isGracefull t2 == True -- t2 = B
    print $ isGracefull t3 == False -- t3 = C

data NTree a = Nil | Node a [NTree a]
 deriving(Show)

t1 = Node  1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]
t2 = Node 7 [Node 3 [ Node 9 [Node 5 [Nil], Node 1 [Nil]]]]
t3 = Node 1 [Node 3 [Nil], Node 5 [Node 42 [Nil]], Node 7 [Nil], Node 9 [Nil]]

isGracefull :: NTree Int -> Bool
isGracefull Nil = True 
isGracefull (Node value children) = all checkChild children && all isGracefull children
 where
     checkChild ::NTree Int  -> Bool
     checkChild Nil = True 
     checkChild (Node valueC _) =  even $ abs( value - valueC)


