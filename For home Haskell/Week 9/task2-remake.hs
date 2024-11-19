import Data.List
import Data.Char

main :: IO()
main = do
   print $ "Tests for sumSpecialPrimes:"
   print $ sumSpecialPrimes 5 2 == 392
   print $ sumSpecialPrimes 5 3 == 107
   print $ sumSpecialPrimes 10 3 == 462

   print $ "Digits function tests:"
   print $ digits 450 == [4,5,0]
   print $ digits 6308 == [6,3,0,8]
   print $ digits 1412412 == [1, 4, 1, 2, 4, 1, 2]

isPrime :: Int -> Bool
isPrime n = length (filter (\ x -> mod n x == 0) [1 .. n] ) == 2

-- We do not need the "digits" function anymore but I still remade it in one line (looks much better :D) + I included some tests for it
digits :: Int -> [Int]
digits x = map digitToInt (show x)

-- hadDigitD in one line with elem, show and intToDigit
hasDigitD :: Int -> Int -> Bool
hasDigitD n d = elem (intToDigit d) (show n)

-- I am not really sure if this is the infinite list you are looking for but I read about this implementation online and it looks good to me :D
infiniteList :: (Num a) => a -> [a]
infiniteList n = n : infiniteList (n + 1)

-- Here is sumSpecialPrimes in one line with infinite list, take && list comprehension
sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [x | x <- infiniteList 1, hasDigitD x d && isPrime x]