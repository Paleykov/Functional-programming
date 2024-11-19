import Data.Char

main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3

    print $ average numberBTree == 16.22
    --print $ average charBTree -- should not work
 
    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    -- The test examples were wrong (we need to compare the trees with areEqual since there is no instance for (Eq (BTree (Int, Char))) )
    print $ areEqual (setLevels numberBTree) (Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil))))
    print $ areEqual (setLevels charBTree) (Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil)))

    print $ areEqual (mirrorTree numberBTree) (Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil))))
    print $ areEqual (mirrorTree charBTree) (Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil)))
   
data BTree a = Nil | Node a (BTree a) (BTree a)

numberBTree :: (Num a) => BTree a
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

-- HELPER FUNCTIONS: 
sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

size :: BTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

rounder :: (RealFrac a) => a -> a
rounder x = fromIntegral (round (x * 100)) / 100
-- END OF HELPER FUNCTIONS;

height :: BTree a -> Int
height Nil = 0
height (Node value left right) = max (1 + (height left)) (1 + (height right))

average :: (Fractional a, RealFrac a) => BTree a -> a
average Nil = 0
average tree = rounder $ sumTree tree / fromIntegral (size tree)

sumLeaves :: (Integral a) => BTree a -> a
sumLeaves Nil = 0;
sumLeaves (Node value Nil Nil) = value;
sumLeaves (Node value left right) = sumLeaves left + sumLeaves right

areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual Nil Nil = True
areEqual (Node value1 left1 right1) (Node value2 left2 right2) = value1 == value2 && areEqual left1 left2 && areEqual right1 right2
areEqual _ _ = False

setLevels :: BTree a -> BTree (Int, a)
setLevels tree = helper 0 tree
 where
    helper :: Int -> BTree a -> BTree (Int, a)
    helper _ Nil = Nil
    helper level (Node value left right) = Node (level, value) (helper (level + 1) left) (helper (level + 1) right)

mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node value left right) = Node value (mirrorTree right) (mirrorTree left)