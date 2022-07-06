import Data.List (delete)

data BB a = L | K a (BB a) (BB a) deriving Show

-- a)
--            5
--        3       7
--    1               12
--                  9 
b = K 5 
        (K 3 
            (K 1 L L) 
            L
        ) 
        (K 7 
            L 
            (K 12 
                (K 9 L L) 
                L
            )
        )

-- b)
instance Foldable BB where
    foldr f acc L = acc
    foldr f acc (K w l r) = foldr f (f w (foldr f acc r)) l

minVal :: Ord a => BB a -> a
minVal = foldr1 (\x acc -> if x < acc then x else acc)

minVal' :: Ord a => BB a -> a
minVal' L = error "empty tree"
minVal' (K w L L) = w
minVal' (K w L r) = min w (minVal' r)
minVal' (K w l L) = min w (minVal' l)
minVal' (K w l r) = min w (min (minVal' r) (minVal' l))

maxVal :: Ord a => BB a -> a
maxVal tree = foldr1 (\x acc -> if x > acc then x else acc) tree

isBinarySearchTree :: Ord a => BB a -> Bool
isBinarySearchTree L = True
isBinarySearchTree (K _ L L) = True
isBinarySearchTree (K w L r) = isBinarySearchTree r && w < minVal r
isBinarySearchTree (K w l L) = isBinarySearchTree l && w > maxVal l
isBinarySearchTree (K w l r) = isBinarySearchTree r && w < minVal r && isBinarySearchTree l && w > maxVal l

isBinarySearchTree' :: Ord a => BB a -> Bool
isBinarySearchTree' = isSorted . treeToList

treeToList :: BB a -> [a]
treeToList = foldr (\x acc -> x:acc) []

isSorted :: Ord a => [a] -> Bool
isSorted (x:y:xs) = x < y && isSorted(y:xs)
isSorted _ = True

-- c)
depth :: (Num a, Ord a) => BB t -> a
depth L = 0
depth (K _ L L) = 1
depth (K _ l r) = 1 + max (depth l) (depth r)

-- d)
insert :: Ord a => a -> BB a -> BB a
insert x L = K x L L
insert x (K w l r) | x > w = K w l (insert x r)
                   | otherwise = K w (insert x l) r

-- e)
buildTree :: Ord a => [a] -> BB a
buildTree xs = foldr (\x acc -> insert x acc) L (reverse xs)

-- f)
instance (Eq a, Ord a) => Eq (BB a) where
    (==) b1 b2 = equalValues (treeToList b1) (treeToList b2)  && isBinarySearchTree b1 && isBinarySearchTree b2
    
equalValues :: Eq a => [a] -> [a] -> Bool
equalValues [] [] = True
equalValues xs [] = False
equalValues [] ys = False
equalValues (x:xs) ys = x `elem` ys && equalValues xs (delete x ys)