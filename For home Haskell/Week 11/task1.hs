main :: IO()
main = do
    print $ prodEvens [1,2,3,4,5,6] 
    print $ prodEvens [7.66 , 7, 7.99, 7]

prodEvens :: (Num a) => [a] -> a
prodEvens = foldr (\(pos, x) acc -> if even pos then x * acc else acc) 1 . zip [0 ..]