main :: IO()
main = do
    print $ areEqual (convert tree) (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree = Nil | Node Int BTree BTree
 deriving (Show)

areEqual :: BTree -> BTree -> Bool
areEqual Nil Nil = True
areEqual (Node value1 left1 right1) (Node value2 left2 right2) = value1 == value2 && areEqual left1 left2 && areEqual right1 right2
areEqual _ _ = False

getAllElements :: BTree -> [Int]
getAllElements Nil = []
getAllElements (Node value left right) = [value] ++ getAllElements left ++ getAllElements right

convert :: BTree -> BTree
convert Nil = Nil
convert (Node value left right) = Node (sum (filter (\x -> x >= value) list)) (convert left) (convert right)
 where
    list = getAllElements tree

tree :: BTree 
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))