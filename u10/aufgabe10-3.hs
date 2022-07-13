-- a)
data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- b)
scale :: (Functor f, Num b) => b -> f b -> f b 
scale factor = fmap (* factor)

-- c)
concatL :: List a -> List a -> List a
concatL Nil ys = ys
concatL xs Nil = xs
concatL (Cons x xs) ys = Cons x (concatL xs ys)

instance Applicative List where
  pure x = Cons x Nil
  
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = concatL (fmap f xs) (fs <*> xs)

zipWith f xs ys = (fmap f xs) <*> ys 