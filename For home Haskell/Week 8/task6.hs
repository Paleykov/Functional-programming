main :: IO()
main = do  
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

isPrimeG :: Int -> Bool
isPrimeG n = length (helper 1 []) == 2 
 where
    helper :: Int -> [Int] -> [Int]
    helper currN list 
     | currN == n = currN : list
     | mod n currN == 0 = helper (currN + 1) (currN : list)
     | otherwise = helper (currN + 1) list

isPrimeLC :: Int -> Bool
isPrimeLC n = length (filter (\ x -> mod n x == 0) [1 .. n] ) == 2