main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True  

isPerfect :: Int -> Bool
isPerfect n = sum [ x | x <- [1 .. n], mod n x == 0] == 2*n
