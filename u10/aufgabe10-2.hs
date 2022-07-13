-- a)
xor :: Integer -> Integer -> Integer
xor 0 0 = 0
xor 0 1 = 1 
xor 1 0 = 1 
xor 1 1 = 0

xor' :: Integer -> Integer -> Integer
xor' x y | x == y = 0
         | otherwise = 1

-- c)
instance Monoid Integer where
    mempty = 0
    mappend = (<>)

instance Semigroup Integer where
    (<>) = xor