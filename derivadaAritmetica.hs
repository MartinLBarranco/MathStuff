--module derivadaAritmetica (derArit, deriLista, esMayor, derivadaSuperior) where
--No me deja hacer el module :(




import Data.Numbers.Primes (isPrime, primeFactors)




{-
DERIVADA ARITMÉTICA:

-)D(1) = 0
-)Si p es primo, D(p) = 1
-)Regla del producto: D(p*q) = D(p)*q + q*D(p)
-)Regla de la potencia: D(p^n) = n*D(p)*p^(n-1)
-}

-- ----------------------------------------------------------------------
-- deriArit es la función que calcula la derivada aritmética de un número.
--Si n es primo, el resultado es 1 (por definición), si es compuesto, entonces
--se llama a la función auxiliar deriLista que hace la derivada del producto
--sobre los elementos de la lista.
-- ----------------------------------------------------------------------
derArit :: Integer -> Integer 
derArit n | n == 1 = 0
          | isPrime n = 1
          | otherwise = deriLista (primeFactors n) 



--Función auxiliar para calcular la derivada aritmetica de un num. compuesto
--Creo que lo de derArit de abajo del todo puede quitarse, ya que la lista es
--de numeros primos, y su derivada es 1.
deriLista :: [Integer] -> Integer
deriLista [] = 0
deriLista (x:xs) = (derArit x) * product xs + x * (deriLista xs)


-- ----------------------------------------------------------------------
--Esta función verifica si la derivada de n es mayor o igual que n.
--Si aplicamos esta función a los primeros 100 números, tenemos esto:

--[False,False,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,True,False,True,False,False,False,True,False,False,True,True,False,True,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,True,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,False,False,True,False,False,False,True,True,False,False,True,False,False,False,True,False,True,False,True,False,False,False,True,False,False,False,True]

--Por lo que vemos que la mayoría de los números sun mayores que sus derivadas
--Vamos algunos ejemplos de números que son mayores o iguales que n:
--[4,8,12,16,18,20,24,27,28,30,32,36,40,44,48,52,54,56,60,64,68,72,76,80,81,84,88,90,92,96,100]
-- Observesa que todos son pares menos 27, sería curioso ver que 27 es el unico
-- numero impar que verifica esto.
-- ----------------------------------------------------------------------

esMayor :: Integer -> Bool
esMayor n = derArit n >= n

-- ----------------------------------------------------------------------

--Vamos a definar la derivada de orden superior. Es decir, iterar las derivadas.
--El primer argumento es el numero a derivar, el segundo, las veces que vamos a derivar.

-- ----------------------------------------------------------------------

derivadaSuperior :: Integer -> Integer -> Integer
derivadaSuperior n 0 = n
derivadaSuperior n k = derivadaSuperior (derArit n) (k-1)

-- ----------------------------------------------------------------------

--Vamos a ver ahora la integral aritmética de un número.
--Tengo que resolver esta ecuación.

-- ----------------------------------------------------------------------


inteAri :: Integer -> Maybe Integer
inteAri n | n == 0 = Just 1
          | n == 1 = Nothing
          | otherwise = undefined

-- ----------------------------------------------------------------------
