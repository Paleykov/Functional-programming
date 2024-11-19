main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

evenDroper :: [a] -> [a]
evenDroper [] = []
evenDroper [_] = []
evenDroper (x:_:xs) = x : evenDroper xs

getOddCompositionValue :: [a -> a] -> a -> a
getOddCompositionValue = foldr (.) id . evenDroper

