import Data.List
main :: IO()
main = do
    print $ genWords t1 == ["acf","acd","abe","cf","cd","f","d","be","e"]
    print $ genWords t2 == ["acd","ab","cd","d","b"]
    print $ genWords t3 == ["abdh","abdi","abe","acf","acg","bdh","bdi","be","dh","di","h","i","e","cf","cg","f","g"]
    
data BTree = Nil | Node Char BTree BTree

t1 :: BTree 
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree 
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree 
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil)) 

genWords :: BTree -> [String]
genWords (Node value Nil Nil) = [[value]]
genWords Nil = [""]
genWords tree@(Node value left right) = filter (containsWord tree) $ (map (value: ) $ genWords left ++ genWords right) ++ genWords left ++ genWords right



containsWord :: BTree -> String -> Bool 
containsWord Nil _ = False 
containsWord _ "" = False 
containsWord (Node value Nil Nil) [x] = x == value
containsWord (Node value left right) word@(x:xs)
 | x == value = helper left xs || helper right xs
 | otherwise = containsWord left word || containsWord right word
  where
      helper :: BTree -> String -> Bool
      helper (Node char Nil Nil) [x] = char == x
      helper (Node char left right) (x:xs) = char == x && (helper left xs || helper right xs)
      helper _ _ = False
      
