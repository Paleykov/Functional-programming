import Data.Char

main :: IO()
main = do
    print $ squareDigits 9119
    print $ squareDigits (-9119)

squareDigits :: Integer -> Integer
squareDigits n
  |n < 0 = negate $ read $ concat $ map (show . (^2) . digitToInt) $ show $ abs n
  |otherwise = read $ concat $ map (show . (^2) . digitToInt) $ show n