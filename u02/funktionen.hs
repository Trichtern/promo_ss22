alleGleich :: Eq a => a -> a -> a -> Bool
alleGleich x y z = (x == y && y == z);

ungerade :: Integral a => a -> Bool
ungerade x = x `mod` 2 /= 0;

gerade :: Integral a => a -> Bool
gerade x = x `mod` 2 == 0;

ungeradeZahlen :: [Integer] -> [Integer]
ungeradeZahlen xs = [x | x <- xs, ungerade x];

geradeZahlen2 :: [Integer] -> [Integer]
geradeZahlen2 xs = [if ungerade (x) then 2 * x else x | x <- xs];

div7Rest5 :: Integer -> Integer -> [Integer]
div7Rest5 x1 x2 = [x | x <- [x1..x2], x `mod` 7 == 5];

length' :: [a] -> Integer
length' xs = sum([1 | _ <- xs]);

dreifach :: Integer -> Integer
dreifach zahl = head([3 * x | x <- [zahl]]);

nurGrossBuchstaben :: [Char] -> [Char]
nurGrossBuchstaben xs = [x | x <- xs];

faktoren :: Integer -> [Integer]
faktoren zahl = [x | x <- [2 .. zahl `div` 2], zahl `mod` x == 0];

faktoren2 :: Integer -> Integer -> Integer
faktoren2 x1 x2 = [x | x <- faktoren x1] !! 0;