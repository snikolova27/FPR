main :: IO()
main = do
    print $ matching "1234"  == []-- ➝ []
    print $ matching ",[.[-],]"  == [(1,7),(3,5)]-- ➝ [(3,5),(1,7)] or [(1,7),(3,5)]
    print $ matching ",+[-.,+]" == [(2,7)]-- ➝ [(2,7)]
    print $ matching "[][]" == [(0,1),(2,3)]-- ➝ [(0,1),(2,3)]
    -- print $ findOpeningBrackets  "1234" 
    -- print $ findOpeningBrackets ",[.[-],]" 
    -- print $ findClosingBrackets ",[.[-],]" 
    print $ findOpeningBrackets "[[]][]" 
    print $ findClosingBrackets "[[]][]" 

findOpeningBrackets :: String -> [Int] 
findOpeningBrackets xs = helper 0 xs
  where
      helper :: Int-> String -> [Int]
      helper current [] = []
      helper current (x:xs)
       | x == '[' = current : helper (current + 1) xs
       | otherwise = helper (current + 1) xs

findClosingBrackets :: String -> [Int] 
findClosingBrackets xs = helper 0 xs
  where
      helper :: Int-> String -> [Int]
      helper current [] = []
      helper current (x:xs)
       | x == ']' = current : helper (current + 1) xs
       | otherwise = helper (current + 1) xs

matching :: String -> [(Int, Int)]
matching str = helper firstBrackets closingBrackets
  where
      firstBrackets = findOpeningBrackets str
      closingBrackets = findClosingBrackets str
      helper :: [Int] -> [Int] -> [(Int, Int)]
      helper [] []  = []
      helper open@(o:opening) close@(c:closing) 
       | last open > c = (o,c) : helper opening closing
       | otherwise = (o, firstInRev) : helper opening (tail revClosing)
         where
           revClosing = reverse close
           firstInRev = head revClosing
