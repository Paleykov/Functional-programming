import Data.List

main :: IO()
main = do
    print $ mergeLinearRec [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 4, 5, 6]
    print $ mergeLinearRec [1, 2, 3] [2] == [1, 2, 3]
    print $ mergeLinearRec [1, 2, 3] [7, 8, 9] == [1, 2, 3, 7, 8, 9]
    print $ mergeLinearRec [2, 3, 4, 5, 6] [1, 2, 3] == [1,2,3,4,5,6]
    print $ mergeLinearRec [2] [1, 2, 3] == [1,2,3]
    print $ mergeLinearRec [7, 8, 9] [1, 2, 3] == [1,2,3,7,8,9]
    print $ mergeLinearRec [7, 9, 11] [8, 10, 12] == [7,8,9,10,11,12]
    print $ mergeXs [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 4, 5, 6]
    print $ mergeXs [1, 2, 3] [2] == [1, 2, 3]
    print $ mergeXs [1, 2, 3] [7, 8, 9] == [1, 2, 3, 7, 8, 9]
    print $ mergeXs [2, 3, 4, 5, 6] [1, 2, 3] == [1,2,3,4,5,6]
    print $ mergeXs [2] [1, 2, 3] == [1,2,3]
    print $ mergeXs [7, 8, 9] [1, 2, 3] == [1,2,3,7,8,9]
    print $ mergeXs [7, 9, 11] [8, 10, 12] == [7,8,9,10,11,12]

mergeLinearRec :: Ord a => [a] -> [a] -> [a]
mergeLinearRec [] ys = nub ys
mergeLinearRec xs [] = nub xs
mergeLinearRec (x:xs) (y:ys)
  | x == y    = x : mergeLinearRec xs ys
  | x < y     = x : mergeLinearRec xs (y:ys)
  | otherwise = y : mergeLinearRec (x:xs) ys

mergeXs :: Ord a => [a] -> [a] -> [a]
mergeXs xs ys = nub $ sort $ xs ++ ys