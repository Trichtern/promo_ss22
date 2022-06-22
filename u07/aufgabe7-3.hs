-- a)
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' _ [] = False
any'' p (x:xs) | p x = True
               | otherwise = any'' p xs

-- b)
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' _ [] = True
all'' p (x:xs) | p x = all' p xs
               | otherwise = False

-- c)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []