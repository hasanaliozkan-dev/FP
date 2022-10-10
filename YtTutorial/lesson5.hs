

ones :: [Int]
ones = 1 : ones 

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n-1) x

--primes :: t
--primes = sieve[2..]

{-
twins :: [(a, a)]
twins = filter twin (zip primes(tail primes))
-}
--sieve :: Integral a => [a] -> [a]
--sieve  (p:xs) = p : [x| x<- xs, mod x p /= 0]

--twin :: (Eq a, Num a) => (a, a) -> Bool
--twin (x,y) = y == x+2
