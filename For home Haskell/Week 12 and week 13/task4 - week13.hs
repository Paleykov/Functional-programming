import Data.List

main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False

data BTree = Nil | Node Int BTree BTree

t1 :: BTree 
t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 :: BTree 
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil)(Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))

t3 :: BTree 
t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t4 :: BTree 
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

leaves :: BTree -> [Int]
leaves Nil = []
leaves (Node value Nil Nil) = [value]
leaves (Node _ left right) = leaves left ++ leaves right

leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual tree1 tree2 = sort (leaves tree1) == sort (leaves tree2)