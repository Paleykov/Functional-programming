import Data.List
import Data.Char

main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6

-- Remade listToNumber with read + map intToDigit
listToNumber :: [Int] -> Int
listToNumber x = read $ map intToDigit x

-- Removed head
reverseOrdSuff :: Int -> Int
reverseOrdSuff n = listToNumber $ reverse $ helper (div n 10) [mod n 10]
    where
        helper :: Int -> [Int] -> [Int]
        helper n (x:xs)
         | n < 10 && n > x = n : (x:xs)
         | n < 10 && n < x = (x:xs)
         | mod n 10 > x = helper (div n 10) (mod n 10 : (x:xs))
         | otherwise = (x:xs)

