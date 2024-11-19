main :: IO()
main = do
    print $ matching "1234"
    print $ matching ",[.[-],]"
    print $ matching ",+[-.,+]"
    print $ matching "[][]"

matching :: String -> [(Int, Int)]
matching xs = helper (zip [0 ..] xs) [] []
  where
    helper [] _ result = result
    helper ((index, '['):rest) stack result = helper rest (index : stack) result
    helper ((index, ']'):rest) (x:xs) result = helper rest xs ((x, index) : result)
    helper (_:rest) stack result = helper rest stack result
    