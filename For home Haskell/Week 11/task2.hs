main :: IO ()
main = do
    print $ controller "" -- → ""
    print $ controller ".........." -- "0000000000"
    print $ controller "P...." -- "12345"
    print $ controller "P.P.." -- "12222"
    print $ controller "..P...O..." -- "0012343210"
    print $ controller "P......P......" -- "12345554321000"
    print $ controller "P.P.P...." -- "122234555"
    print $ controller ".....P.P........P...." -- "000001222222222234555"
    print $ controller ".........." -- "0000000000"
    print $ controller "P.." -- "123"
    print $ controller "P...." -- "12345"
    print $ controller "P......P......" -- "12345554321000"
    print $ controller "P.P.." -- "12222"
    print $ controller "P.P.P...." -- "122234555"
    print $ controller ".....P.P........P...." -- "000001222222222234555"
    print $ controller ".....P......P.P..P...." -- "0000012345554333321000"
    print $ controller "P.O...." -- "1210000"
    print $ controller "P......P.O...." -- "12345554345555"
    print $ controller "P..OP..P.." -- "1232222100"
    print $ controller "P......P..OP..P..." -- "123455543233334555"
    print $ controller "..P...O....." -- "001234321000"

controller :: String -> String
controller xs = reverse $ concat $ map (show) $ helper xs [] 0 False
    where
        helper :: String -> [Int] -> Int -> Bool -> [Int]
        helper [] result _ _ = result
        helper (x:xs) [] _ _
         |x == '.' = helper xs [0] 0 False
         |otherwise = helper xs [1] 1 True
        helper (x:xs) (r:result) difference cap
         |x == 'P' && r == 0 = helper xs (r + 1 : r : result) 1 True
         |x == 'P' && r == 5 = helper xs (r - 1 : r : result) (-1) False
         |x == 'P' && difference /= 0 = helper xs (r : r : result) 0 cap
         |x == 'P' && cap == True = helper xs (r + 1 : r : result) 1 cap 
         |x == 'P' = helper xs (r - 1 : r : result) (-1) cap 
         |x == 'O' && cap == True = helper xs (r - difference : r : result) (-difference) False
         |x == 'O' = helper xs (r - difference : r : result) (-difference) True
         |x == '.' && r + difference <= 5 && r + difference >= 0 = helper xs (r + difference : r : result) difference cap
         |otherwise =  helper xs (r : r : result) difference cap

