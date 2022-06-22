data ML a = E | L a (ML a)

-- a)
liste :: ML Int
liste = L 1 (L 2 (L 3 (L 4 E)))

-- b)
myHead :: ML a -> a
myHead E = error "empty list"
myHead (L x _) = x

-- c)
myTail :: ML a -> ML a
myTail E = error "empty list"
myTail (L _ xs) = xs

-- d)
myAdd :: Num a => ML a -> ML a -> ML a
myAdd E _ = E
myAdd _ E = E
myAdd (L x xs) (L y ys) = L (x + y) (myAdd xs ys)

-- e)
myAppend :: ML a -> ML a -> ML a
myAppend xs E = xs
myAppend E ys = ys
myAppend (L x xs) ys = L x (myAppend xs ys)

-- f)
toString :: Show a => ML a -> String
toString E = ""
toString (L x E) = show x
toString (L x xs) = show x ++ ", " ++ toString xs 

-- f) alternativ
instance Show a => Show (ML a) where
    show E = ""
    show (L x E) = show x
    show (L x xs) = show x ++ ", " ++ show xs 