import Data.Char (toUpper)

-- a)
length' :: [a] -> Int
length' = foldr (\ _ acc -> acc + 1) 0

-- b)
capitalizedInitials :: String -> String
capitalizedInitials xs = map (\x -> toUpper (head x)) (words xs)

-- c)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- d)
filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' f = foldr (\x acc -> if (f x) then x : acc else acc) []

-- e)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x acc -> if (f x) then x : acc else []) []