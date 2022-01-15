main :: IO()
main = do

   print $ grandchildrenIncreased t1 == True
   print $ grandchildrenIncreased t2 == False

    
data BTree = Nil | Node Int BTree BTree 
 deriving(Show, Eq)

t1 = Node 1 (Node (-1) (Node 2 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)) (Node (-1) Nil Nil)
t2 = Node 1 (Node 2 (Node 1 Nil Nil) (Node 1 (Node 10 Nil Nil) Nil)) (Node 3 Nil Nil)

getGrandchildren:: BTree -> [Int]
getGrandchildren Nil = []
getGrandchildren (Node _ left right) = getChildren left ++ getChildren right

getChildren :: BTree -> [Int]
getChildren Nil = []
getChildren (Node _ Nil Nil) = []
getChildren (Node _ Nil (Node valueR _ _ ) ) = [valueR]
getChildren (Node _ (Node valueL _ _ ) Nil) = [valueL]
getChildren (Node _  (Node valueL _ _ ) (Node valueR _ _ )) = [valueL, valueR]

haveGrandchildrenIncreased :: BTree -> Bool 
haveGrandchildrenIncreased Nil = True 
haveGrandchildrenIncreased (Node _ Nil Nil) = True 
haveGrandchildrenIncreased t@(Node grandfather _ _ ) = all (\ x -> x - grandfather >=1) (getGrandchildren t )

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased t@(Node _ left right) = haveGrandchildrenIncreased t && haveGrandchildrenIncreased left && haveGrandchildrenIncreased right