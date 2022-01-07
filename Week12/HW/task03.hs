main :: IO()
main = do
    print colourBTree
    print $ highest Red colourBTree  == 4
    print $ highest Green colourBTree == 3
    print $ highest Blue colourBTree  == 4
  --  print $ levelsWithColor Blue colourBTree

data Color  = Red | Green | Blue 
 deriving (Show, Eq)

data BTree a = Nil | Node a (BTree a) (BTree a) 
 deriving (Show, Eq)

colourBTree = Node Blue (Node Green (Node Blue (Node Red Nil Nil) Nil) (Node Blue Nil Nil)) (Node Red (Node Green (Node Blue Nil Nil) Nil) (Node Red Nil Nil))

highest :: Color -> BTree Color -> Int 
highest _ Nil = 0
highest color tree = maximum (levelsWithColor color tree)

levelsWithColor :: Color -> BTree Color -> [Int]
levelsWithColor _ Nil = []
levelsWithColor color tree = helper 1 tree
 where 
     helper :: Int ->  BTree Color -> [Int]
     helper _ Nil = []
     helper current (Node colorT left right) 
      | colorT == color = [current] ++ helper (current + 1)  left ++ helper (current + 1)  right
      | otherwise = helper (current + 1) left ++ helper (current + 1)  right
