import Graphics.Rendering.OpenGL (MatrixMode(Color))
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
levelsWithColor color tree = helper 1 (height tree) tree
 where 
     helper :: Int -> Int-> BTree Color -> [Int]
     helper _ _ Nil = []
     helper current h (Node colorT left right) 
      | colorT == color && current <= h = [current] ++  helper (current +1) h left ++ helper (current +1 ) h right
      | otherwise = helper (current +1) h left ++ helper (current +1 ) h right


getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

height :: BTree Color -> Int 
height Nil = 0
height (Node _ left right) = max ( 1 + height left) (1 + height right)