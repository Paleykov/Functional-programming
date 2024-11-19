import Data.Char
import Data.List

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

-- Removed the unnecessary brackets
isDuplicate :: Char -> Char -> Bool
isDuplicate c1 c2 = c1 /= c2 && toLower c1 == toLower c2

-- Removed any usage of head and tail
reduceStr :: String -> String
reduceStr [] = []
reduceStr [x] = [x]
reduceStr ss = reverse $ helper ss ""
    where
        helper [] resSS = resSS
        helper (x:currSS) [] = helper currSS [x]
        helper (x:currSS) (y:resSS)
         | not (null resSS) && isDuplicate y x = helper currSS resSS
         | otherwise = helper currSS (x : (y:resSS))
    