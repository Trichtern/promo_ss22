-- a)
data Retrievable a = NotAvailable | Present a deriving Show

-- b)
instance Functor Retrievable where
    fmap _ NotAvailable = NotAvailable
    fmap f (Present a) = Present (f a)

-- c)
instance Applicative Retrievable where
    pure x = Present x
    NotAvailable <*> _ = NotAvailable
    _ <*> NotAvailable = NotAvailable
    (Present f) <*> (Present x) = Present (f x)

-- d)
instance Monad Retrievable where
    return = pure
    NotAvailable >>= _ = NotAvailable
    (Present a) >>= f = f a