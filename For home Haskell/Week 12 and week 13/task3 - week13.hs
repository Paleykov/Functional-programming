import Data.List (sortOn)

main :: IO()
main = do
    print $ toBinaryIndexed tree -- == Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

tree :: BTree Int
tree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

sortDescending :: (Ord a, Num a) => [(a, Int)] -> [(a, Int)]
sortDescending = sortOn (negate . fst)

insert :: (Ord a) => BTree (a, Int) -> (a, Int) -> BTree (a, Int)
insert Nil (x,y) = Node (x,y) Nil Nil
insert (Node (x,y) left right) (t,p)
 | t < x = Node (x,y) (insert left (t,p)) right
 | otherwise = Node (x,y) left (insert right (t,p))

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

traverseBFS :: BTree a -> [a]
traverseBFS tree = helper [tree]
  where
    helper :: [BTree a] -> [a]
    helper [] = []
    helper (Nil : rest) = helper rest
    helper (Node value left right : rest) = value : helper (rest ++ [left, right])

indexList :: [a] -> [(a, Int)]
indexList xs = zip xs [0..]

toBinaryIndexed :: (Ord a) => BTree a -> BTree (a,Int)
toBinaryIndexed tree =  {--foldl insert Nil $--} indexList (traverseDFS tree)

