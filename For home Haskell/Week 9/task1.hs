import Data.List

main :: IO()
main = do
    print $ addN [1, 2, 3, 4, 5] 9999999999999999999999  == [10000000000000000000000,10000000000000000000001,10000000000000000000002,10000000000000000000003,10000000000000000000004]
    print $ sqAddN [1, 2, 3, 4, 5] 5 == [36,49,64,81,100]
    print $ divByN [1, 2, 3, 4, 5] 5 == [0.2, 0.4, 0.6, 0.8, 1.0]
    print $ divByN [1, 2, 3, 4, 5] (-5) == [-0.2, -0.4, -0.6, -0.8, -1.0]
    print $ filterByN [1, 2, 3, 4, 5] 3 == [3,4,5]

addN :: (Num a) => [a] -> a -> [a]
addN = flip (map . (+))

sqAddN :: (Num a) => [a] -> a -> [a]
sqAddN = (map (^2) .) . addN

divByN :: (Fractional a) => [a] -> a -> [a]
divByN = flip $ map . (flip (/))

filterByN :: (Ord a) => [a] -> a -> [a]
filterByN = flip $ filter . (flip (>=))




