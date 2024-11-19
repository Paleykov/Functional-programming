import Data.List

main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

data BTree = Nil | Node Int BTree BTree

numberBTree :: BTree 
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

height :: BTree -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

getLevel :: BTree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

levelSum :: BTree -> Int -> Int
levelSum t k = sum $ getLevel t k

getAllLevelsSums :: BTree -> [Int]
getAllLevelsSums tree = reverse $ helper 0 []
 where
    helper :: Int -> [Int] -> [Int]
    helper currLvl result
     | currLvl == (height tree) = result
     | otherwise = helper (currLvl + 1) ((sum (getLevel tree currLvl)) : result)

cone :: BTree -> Bool
cone tree = (getAllLevelsSums tree) == (sort $ getAllLevelsSums tree)


