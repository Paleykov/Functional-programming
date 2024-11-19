main :: IO()
main = do
    print $ ordered t1 -- == True
    print $ ordered t2 -- == False

data BTree a = Nil | Node a (BTree a) (BTree a)

t1 :: BTree (Int, Int)
t1 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (4,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

t2 :: BTree (Int, Int)
t2 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

isSubinterval :: [(Int, Int)] -> Bool
isSubinterval intervals = all (\(x, y) -> any (\(a, b) -> x >= a && y <= b) (tail intervals)) intervals

traverseDFS :: BTree (Int,Int) -> [(Int, Int)]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

ordered :: BTree (Int,Int) -> Bool
ordered tree = isSubinterval nodes
 where
    nodes = traverseDFS tree

