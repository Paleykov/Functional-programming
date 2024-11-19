import Data.Char

main :: IO()
main = do
    print colourBTree
    print $ highest "Red" colourBTree == 4
    print $ highest "Green" colourBTree == 3
    print $ highest "Blue" colourBTree == 4

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving (Show)

-- Helpers 
getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

height :: BTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)
-- End of helpers

colourBTree :: BTree String
colourBTree = Node "Blue" (Node "Green" (Node "Blue" (Node "Red" Nil Nil) Nil) (Node "Blue" Nil Nil)) (Node "Red" (Node "Green" (Node "Blue" Nil Nil) Nil) (Node "Red" Nil Nil))

highest :: String -> BTree String -> Int
highest _ Nil = error "Empty tree"
highest colour tree = helper 0 0
 where
    helper :: Int -> Int -> Int
    helper currentLvl result
     |currentLvl == height tree = result
     |elem colour (getLevel tree currentLvl) = helper (currentLvl + 1) (currentLvl + 1)
     |otherwise = helper (currentLvl + 1) currentLvl