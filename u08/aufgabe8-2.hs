-- a)
data Ordering'  = EQ' | LT' | GT' deriving (Show)

instance Semigroup Ordering' where
    EQ' <> x = x
    x <> _ = x

instance Monoid Ordering' where
    mempty = EQ'
    mappend = (<>)

-- b)
revCompare :: String -> String -> Ordering
revCompare [] [] = EQ
revCompare [] (_:_) = LT
revCompare (_:_) [] = GT
revCompare (x:xs) (y:ys) = (flip compare x y) <> (revCompare xs ys)