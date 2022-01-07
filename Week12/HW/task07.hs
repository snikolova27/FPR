main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: [a->a] -> a-> a
getOddCompositionValue fs x = let listOdd = [fs !! n  | n <- [0..length fs - 1], even n] in foldl(\ acc f -> f acc ) x listOdd