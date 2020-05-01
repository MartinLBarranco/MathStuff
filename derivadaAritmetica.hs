import Data.Numbers.Primes
import Data.List


--Lista de cosas:


deriva :: Integer -> Integer
deriva n | n == 1 || n== 0 = 0
         | isPrime (abs n) = signum (n)
         | otherwise = signum (n) * derivaProd (primeFactors (abs n)) 

derivaProd :: [Integer] -> Integer
derivaProd [] = 0
derivaProd xs = sum [product xs `div` (xs!!(i-1)) | i<-[1..(length xs)]]



plica :: Integer -> [(Integer,Integer)]
plica n = zip [1..n] (map deriva [1..n])

esMayor :: (Integer,Integer) -> Bool
esMayor (a,b) = a>=b

listaMayor :: Integer -> [(Integer,Bool)]
listaMayor n = zip [1..n] (map esMayor (plica n))


derivaSuperior :: Integer -> Integer -> Integer
derivaSuperior n 0 = n
derivaSuperior n k | (deriva n) /= n = derivaSuperior (deriva n) (k-1)
                   | otherwise = n
