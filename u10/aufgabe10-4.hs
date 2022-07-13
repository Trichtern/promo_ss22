-- a)
data Triple a = Triple a a a

instance Show a => Show (Triple a) where
  show (Triple x y z) = show x ++ " " ++ show y ++ " " ++ show z

-- b)
tfst :: Triple a -> a
tfst (Triple x _ _) = x

tsnd :: Triple a -> a
tsnd (Triple _ y _) = y

ttrd :: Triple a -> a
ttrd (Triple _ _ z) = z

-- c)
tripleFromList :: [a] -> Triple a
tripleFromList (x:y:z:_) = Triple x y z

tripleToList :: Triple a -> [a]
tripleToList (Triple x y z) = [x, y, z]

-- d)
x :: Num a => Triple a -> Triple a -> Triple a
x (Triple a b c) (Triple d e f) = Triple (b*f-c*e) (c*d-a*f) (a*e-b*d)

-- e)
instance Functor Triple where
    fmap f (Triple x y z) = Triple (f x) (f y) (f z)

scaMult :: Num a => a -> Triple a -> Triple a
scaMult factor = fmap (* factor)

-- f)
instance Applicative Triple where
    pure x = Triple x x x
    (Triple f1 f2 f3) <*> (Triple x y z) = Triple (f1 x) (f2 y) (f3 z)
    
o :: Num a => Triple a -> Triple a -> a
o t1 t2 = sum $ tripleToList (fmap (*) t1 <*> t2)

tripleAdd :: Num a => Triple a -> Triple a -> Triple a
tripleAdd t1 t2 = tripleFromList $ tripleToList (fmap (+) t1 <*> t2)

tripleSub :: Num a => Triple a -> Triple a -> Triple a
tripleSub t1 t2 = tripleFromList $ tripleToList (fmap (-) t1 <*> t2)

-- g)
tlength :: Floating a => Triple a -> a
tlength t = sqrt $ t `o` t