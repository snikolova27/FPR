main :: IO ()
main = do

    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False

isPalindrome :: Int -> Bool 
isPalindrome x = reversed x 0 == x
 where 
     reversed:: Int -> Int -> Int
     reversed 0 result =  result
     reversed leftOver result = reversed (div leftOver 10) (result *10 + mod leftOver 10)

