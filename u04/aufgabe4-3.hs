-- a)
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' list = len list 0
                where len [] acc = acc
                      len (x:xs) acc = len xs (acc + 1)

-- b)
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) elem | x == elem = True
                     | otherwise = contains xs elem

contains' :: Eq a => [a] -> a -> Bool
contains' xs elem = con xs elem
                    where con [] elem = False
                          con (x:xs) elem | x == elem = True
                                          | otherwise = con xs elem

-- c)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' xs = hreverse xs
               where hreverse [] = []
                     hreverse (x:xs) = hreverse xs ++ [x]

-- d)
append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' xs [] = xs
append' xs (y:ys) = append' (xs ++ [y]) (ys)

append'' :: [a] -> [a] -> [a]
append'' xs ys = happend xs ys
                 where happend [] ys = ys
                       happend xs [] = xs
                       happend xs (y:ys) = happend (xs ++ [y]) (ys)