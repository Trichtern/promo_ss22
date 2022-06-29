succ' :: Integer -> Integer
succ' x = x + 1

pred' :: Integer -> Integer
pred' x = x - 1

-- a)
plus :: Integer -> Integer -> Integer
plus 0 b = b
plus a b = plus (pred' a) (succ' b)

minus :: Integer -> Integer -> Integer
minus a 0 = a 
minus a b = minus (pred' a) (pred' b)

-- b)
mult :: Integer -> Integer -> Integer
mult _ 0 = 0
mult a b = plus a (mult a (pred' b))

-- c)
mod' :: Integer -> Integer -> Integer
mod' a b | a < b = a
         | otherwise = mod' (minus a b) b