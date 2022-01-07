import Data.List
main ::IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)]  == [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)]  == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
    print $ getParents [(1, 2, 3), (2, 4, 5)] == [1,2]
    print $ getNodes [(1, 2, 3), (2, 4, 5)] == [1,2,3,2,4,5]

type Parent = Int 
type Child = Int
type Nodes = (Parent, Child, Child)

listLeaves :: [(Parent, Child, Child)] -> [Int]
listLeaves [] = []
listLeaves nodes = let parents = getParents nodes; all =  nub $ getNodes nodes in all \\ parents


getParents :: [(Parent, Child, Child)] -> [Int]
getParents [] = []
getParents nodes  = nub $ map (\ (p, _, _ ) -> p) nodes 

getNodes :: [(Parent, Child, Child)] -> [Int]
getNodes [] = []
getNodes ((x,y,z): xs) = [x,y,z] ++ getNodes xs
 