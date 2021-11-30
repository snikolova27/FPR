---main :: IO()
main = do
    print $ minIf 15 60 == 15
    print $ minIf 60 15 == 15
    print $ minIf (-60) (-15) == (-60)
    print $ minGuard 15 60 == 15
    print $ minGuard 60 15 == 15
    print $ minBuiltIn 60 15 == 15

    print $ lastDigit 154 == 4

    print $ quotientWhole 64 2 == 32
    print $ divWhole 154 17 == 9.058823529411764
    print $ removeLastDigit 154 == 15

    print $ divReal 154.451 10.01 == 15.42967032967033
    print $ quotientReal 154.21 17.17 == 9

    print $ avgWhole 5 1542 == 773.5

    print $ roundTwoDig 3.1413465345321 == 3.14
    print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14 -- partial function application and composition (defining a function at functional level)

minIf :: Int -> Int -> Int
minIf x y = if x < y then x else y

minGuard :: Int -> Int -> Int
minGuard x y
  | x < y = x
  | otherwise = y

minBuiltIn :: Int -> Int -> Int
minBuiltIn = min

lastDigit :: Int -> Int
lastDigit x = mod x 10

quotientWhole :: Int -> Int -> Int
quotientWhole = div

divWhole :: Int -> Int -> Double
divWhole x y = fromIntegral x / fromIntegral y

removeLastDigit :: Int -> Int
removeLastDigit x = div x 10

divReal :: Double -> Double -> Double
divReal x y = x/y

quotientReal::Double -> Double ->Int
quotientReal x y = round $ x / y

avgWhole :: Int->Int->Double 
avgWhole x y = (fromIntegral $ x + y) / 2

roundTwoDig:: Double -> Double 
roundTwoDig x =  (fromIntegral $ round $ x * 100) / 100

roundTwoDigButWithMagic:: Double ->Double 
roundTwoDigButWithMagic  = (/ 100).fromIntegral.round.(*100)