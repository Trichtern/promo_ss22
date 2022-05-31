inverse = [ 1/fromIntegral(n)^2 | n <- [1..]]

pi_approx :: Int -> Double
pi_approx n = sqrt (6 * (sum (take n inverse)))

inverse' :: Int -> [Double] -> [Double]
inverse' 0 acc = acc
inverse' i acc = inverse' (i-1) (1/fromIntegral(i)^2 : acc)

pi_approx' :: Int -> Double
pi_approx' n = sqrt (6 * (sum (inverse' n [])))