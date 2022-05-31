-- Aufgabe 5-1

sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x:sieve' [n | n <- xs, n `mod` x /= 0]