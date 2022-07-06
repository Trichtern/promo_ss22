-- a)
data Operator = Plus | Minus | Times | Division deriving (Show)
data Term a = C a | BinaryTerm Operator (Term a) (Term a) | UnaryTerm Operator (Term a) deriving (Show)

-- b)
eval :: Integral a => Term a -> a
eval (C v) = v
eval (BinaryTerm Plus t1 t2) = eval t1 + eval t2
eval (BinaryTerm Minus t1 t2) = eval t1 - eval t2
eval (BinaryTerm Times t1 t2) = eval t1 * eval t2
eval (BinaryTerm Division t1 t2) = eval t1 `div` eval t2

-- c)
eval (UnaryTerm Minus t) = - (eval t)
eval (UnaryTerm _ _) = error "not defined"

-- d)
simplify :: Term a -> Term a
simplify (C a) = C a
simplify (UnaryTerm Minus (UnaryTerm Minus t)) = simplify t
simplify (BinaryTerm Plus t1 (UnaryTerm Minus t2)) = BinaryTerm Minus (simplify t1) (simplify t2)
simplify (BinaryTerm Minus t1 (UnaryTerm Minus t2)) = simplify (BinaryTerm Plus (simplify t1) (simplify t2))