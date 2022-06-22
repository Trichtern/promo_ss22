-- a)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

-- b)
unzipWith :: (t -> (a, b)) -> [t] -> ([a], [b])
unzipWith f list = unzip' f list ([], []) 
                   where unzip' _ [] (acc1, acc2) = (reverse acc1, reverse acc2)
                         unzip' f (y:ys) (acc1, acc2) = unzip' f ys (fst (f y) : acc1, snd (f y) : acc2)