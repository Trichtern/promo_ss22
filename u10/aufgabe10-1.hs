-- a)
data ComplexNumber = C (Double, Double)

instance Semigroup ComplexNumber where
    C (a, b) <> C (c, d) = C (a + c, b + d)  

instance Monoid ComplexNumber where
    mempty = C (0, 0)
    mappend = (<>)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ (if (b >= 0) then "+" else "-") ++ show (abs b) ++ "i"

-- b)
data RGB = RGB (Integer, Integer, Integer) deriving Show 

instance Semigroup RGB where
    (RGB (r1, g1, b1)) <> (RGB (r2, g2, b2)) = RGB (
                                                   min (r1 + r2) 255, 
                                                   min (g1 + g2) 255, 
                                                   min (b1 + b2) 255
                                               )  

instance Monoid RGB where
    mempty = RGB (0, 0, 0)
    mappend = (<>)